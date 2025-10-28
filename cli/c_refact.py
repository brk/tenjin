import json
from clang.cindex import (
    Index,
    CursorKind,
    Config,
    StorageClass,
    TranslationUnit,
    CompilationDatabase,
    Cursor,
)
from dataclasses import dataclass
import platform
from pathlib import Path
import subprocess

import hermetic
import repo_root
import compilation_database
import batching_rewriter


def create_xj_clang_index() -> Index:
    """Create a clang Index configured to use the hermetic xj-llvm installation."""

    if not Config.loaded:
        xj_llvm = hermetic.xj_llvm_root(repo_root.localdir())

        if platform.system() == "Darwin":
            libclang_path = xj_llvm / "lib" / "libclang.dylib"
        else:
            # Keep this version in sync with the one embedded in pyproject.toml
            libclang_path = xj_llvm / "lib" / "libclang.so.18.1.8"

        Config.set_library_file(libclang_path)

    return Index.create()


def xj_comp_db_from_directory(dir: str) -> compilation_database.CompileCommands:
    return compilation_database.CompileCommands.from_json_file(f"{dir}/compile_commands.json")


def clang_comp_db_from_directory(dir: str) -> CompilationDatabase:
    return CompilationDatabase.fromDirectory(dir)


def parse_translation_unit_with_args(
    index: Index, path: str, args: list[str], in_dir: str | None = None
) -> TranslationUnit:
    """Parse a translation unit with given arguments."""
    args_matching_path = []
    for arg in args:
        if path == arg:
            args_matching_path.append(arg)
        elif in_dir and path == str(Path(in_dir) / arg):
            args_matching_path.append(arg)

    if len(args_matching_path) == 0:
        raise ValueError(f"Could not find source file path '{path}' in args: {args}")

    if len(args_matching_path) > 1:
        raise ValueError(f"Multiple matching source file paths for '{path}' in args: {args}")

    args_sans_path = [arg for arg in args if arg != args_matching_path[0]]

    # We cannot use the args list as-is with `index.parse` method for two reasons:
    #
    # 1. It takes the source file as a separate argument, not in the args list,
    #    and errors if it's present in both places. We can remove it if it's
    #    unambiguously present in the args, but in general we can't distinguish
    #    occurrences as being direct parameters to clang vs flag arguments.
    # 2. When invoking clang directly, it automatically picks up the sysroot
    #    from its configuration file, and automatically uses its own include paths.
    #    When using libclang, we have to specify that information explicitly.
    #
    # An alternative approach, possibly more robust, would be to invoke `clang`
    # to emit a .ast file, which ensures that the AST we end up with is exactly
    # what `clang` would be working with.

    xj_llvm = hermetic.xj_llvm_root(repo_root.localdir())
    sysroot_dir = xj_llvm / "sysroot"

    return index.parse(
        path=path,
        args=[
            f"--sysroot={sysroot_dir.as_posix()}",
            "-isystem",
            (xj_llvm / "lib" / "clang" / "18" / "include").as_posix(),
            *args_sans_path,
        ],
        options=TranslationUnit.PARSE_DETAILED_PROCESSING_RECORD,
    )


def parse_project(
    index: Index,
    compdb: compilation_database.CompileCommands,
) -> dict[str, TranslationUnit]:
    """Parse all translation units in the compilation database.

    Returns a mapping from absolute source file path to TranslationUnit."""
    tus = {}
    for srcfile in compdb.get_source_files():
        cmds = compdb.get_commands_for_path(srcfile)
        if not srcfile.is_absolute():
            srcfile = (cmds[0].directory_path / srcfile).resolve()
        assert len(cmds) == 1, f"Expected exactly one compile command for {srcfile}, got {cmds}"
        parts = cmds[0].get_command_parts()[1:]  # Skip compiler executable
        tu = parse_translation_unit_with_args(
            index,
            srcfile.as_posix(),
            parts,
            in_dir=cmds[0].directory_path.as_posix(),
        )
        if srcfile.is_absolute():
            abs_path = srcfile.resolve()
        else:
            abs_path = (cmds[0].directory_path / srcfile).resolve()
        tus[abs_path] = tu
    return tus


def preprocess_and_create_new_compdb(
    compdb: compilation_database.CompileCommands,
    target_dir: str,
    with_and_without_line_directives: bool = False,
) -> compilation_database.CompileCommands:
    """
    For each TU in compdb, run clang -E to preprocess it into target_dir,
    then create a new compile_commands.json in target_dir that refers to
    the preprocessed .i files.
    """
    new_commands = []
    target_dir_path = Path(target_dir)
    target_dir_path.mkdir(parents=True, exist_ok=True)

    repo_root_path = repo_root.find_repo_root_dir_Path()
    clang_path = hermetic.xj_llvm_root(repo_root.localdir()) / "bin" / "clang"

    for cmd in compdb.commands:
        # 1. Determine paths
        abs_src_path = cmd.absolute_file_path
        try:
            rel_src_path = abs_src_path.relative_to(repo_root_path)
        except ValueError:
            rel_src_path = Path(abs_src_path.name)

        preprocessed_file_path = (target_dir_path / rel_src_path).with_suffix(".i")
        preprocessed_file_path.parent.mkdir(parents=True, exist_ok=True)

        # 2. Run preprocessor
        original_args = cmd.get_command_parts()
        compiler_args = original_args[1:]

        # Remove output file from args
        try:
            o_index = compiler_args.index("-o")
            del compiler_args[o_index : o_index + 2]
        except ValueError:
            pass

        if "-c" in compiler_args:
            compiler_args.remove("-c")

        # Remove source file from args
        temp_args = []
        for arg in compiler_args:
            arg_path = Path(arg)
            if not arg_path.is_absolute():
                arg_path = cmd.directory_path / arg_path

            if arg_path.resolve() != abs_src_path.resolve():
                temp_args.append(arg)
        compiler_args = temp_args

        base_pp_command = [
            str(clang_path),
            "-E",
            str(abs_src_path),
            *compiler_args,
        ]

        subprocess.run(
            [*base_pp_command, "-o", str(preprocessed_file_path)],
            check=True,
            cwd=cmd.directory,
        )

        if with_and_without_line_directives:
            no_lines_path = preprocessed_file_path.with_suffix(".nolines.i")
            refold_map_path = no_lines_path.with_suffix(".refoldmap.json")
            subprocess.run(
                [
                    *base_pp_command,
                    f"--refold-map={refold_map_path.as_posix()}",
                    "--no-line-commands",
                    "-o",
                    str(no_lines_path),
                ],
                check=True,
                cwd=cmd.directory,
            )

        # 3. Create new command for new compdb
        new_args = original_args.copy()

        found = False
        for i, arg in enumerate(new_args):
            arg_path = Path(arg)
            if not arg_path.is_absolute():
                arg_path = cmd.directory_path / arg_path

            if arg_path.resolve() == abs_src_path.resolve():
                new_args[i] = str(preprocessed_file_path)
                found = True
                break

        if not found:
            raise ValueError(
                f"Source file {abs_src_path} not found in command arguments: {original_args}"
            )

        new_commands.append(
            compilation_database.CompileCommand(
                directory=cmd.directory,
                file=str(preprocessed_file_path),
                arguments=new_args if cmd.arguments else None,
                command=" ".join(new_args) if cmd.command else None,
                output=cmd.output,
            )
        )

    # 4. Write new compile_commands.json
    new_compdb = compilation_database.CompileCommands(commands=new_commands)
    new_compdb.to_json_file(target_dir_path / "compile_commands.json")
    return new_compdb


def compute_globals_and_statics_for_project(
    compdb: compilation_database.CompileCommands,
):
    index = create_xj_clang_index()
    tus = parse_project(index, compdb)
    combined = {}
    for tu in tus.values():
        results = compute_globals_and_statics_for_translation_unit(tu)
        common_keys = set(combined.keys()).intersection(set(results.keys()))
        assert not common_keys, f"Duplicate global/static symbols found: {common_keys}"
        combined.update(results)
    return combined


@dataclass
class NamedDeclInfo:
    spelling: str
    file_path: str | None
    decl_start_byte_offset: int
    decl_end_byte_offset: int
    start_line: int
    start_col: int
    end_line: int
    end_col: int


def compute_globals_and_statics_for_translation_unit(
    translation_unit: TranslationUnit,
) -> dict[str, NamedDeclInfo]:
    """Compute globals and static symbols defined in the translation unit."""

    results = {}

    def qualify(cursor):
        names = []
        cur = cursor
        while cur is not None:  # and cur.kind != CursorKind.TRANSLATION_UNIT:
            if cur.spelling:
                names.append(cur.spelling)
            cur = cur.semantic_parent
        if not names:
            return cursor.spelling or ""
        return "::".join(reversed(names))

    def visit(node: Cursor):
        if node.kind in (CursorKind.VAR_DECL, CursorKind.FUNCTION_DECL):
            # Only consider definitions
            try:
                is_def = node.is_definition()
            except Exception:
                is_def = True
            if is_def:
                sc = node.storage_class
                # Include top-level declarations or entities with 'static' storage
                if (
                    # node.semantic_parent.kind == CursorKind.TRANSLATION_UNIT or
                    sc == StorageClass.STATIC
                ):
                    qname = qualify(node)
                    extent = node.extent
                    start = extent.start
                    end = extent.end
                    file_path = start.file.name if start.file else None
                    results[qname] = NamedDeclInfo(
                        spelling=node.spelling,
                        file_path=file_path,
                        decl_start_byte_offset=start.offset,
                        decl_end_byte_offset=end.offset,
                        start_line=start.line,
                        start_col=start.column,
                        end_line=end.line,
                        end_col=end.column,
                    )
        for child in node.get_children():
            visit(child)

    visit(translation_unit.cursor)
    return results


def localize_mutable_globals(json_path: Path, compdb: compilation_database.CompileCommands):
    # Here is an example of the data output by `cc2json`:
    # {
    # "mutated_or_escaped_global": [
    #     "a_1.foo_xjtr_1",
    #     "a_2.foo_xjtr_2",
    #     "b_1.foo_xjtr_0"
    # ],
    # "call_graph_components": [
    #     {
    #         "call_sites": [
    #             {
    #                 "line": 10,
    #                 "col": 25,
    #                 "p": "main",
    #                 "uf": "main.c"
    #             },
    #             {
    #                 "line": 11,
    #                 "col": 25,
    #                 "p": "main",
    #                 "uf": "main.c"
    #             },
    #             {
    #                 "line": 12,
    #                 "col": 25,
    #                 "p": "main",
    #                 "uf": "main.c"
    #             }
    #         ],
    #         "call_targets": [
    #             "<llvm-link>:a_1"
    #         ],
    #         "all_mutable": true
    #     },
    #     {
    #         "call_sites": [ ... ] }
    # ],
    # "unique_filenames": {
    # "a.c": {"directory": "/home/brk/tenjin/ju_xjres2/c_03_run_cclzyerpp_analysis", "filename": "a.c"},
    # "b.c": {"directory": "/home/brk/tenjin/ju_xjres2/c_03_run_cclzyerpp_analysis", "filename": "b.c"},
    # "main.c": {"directory": "/home/brk/tenjin/ju_xjres2/c_03_run_cclzyerpp_analysis", "filename": "main.c"}
    # },
    # "mutable_global_tissue": {
    #     "directly_accesses": [
    #     "a_1",
    #     "a_2",
    #     "b_1"
    #     ],
    #     "tissue": [
    #     "main",
    #     "a_1",
    #     "a_2",
    #     "b_1"
    #     ]
    # }
    # }

    # Assume this value holds an instance of the above JSON type
    j: dict = json.load(json_path.open("r"))

    def un_uf(uf: str) -> str:
        nonlocal j
        v = j["unique_filenames"][uf]
        return v["directory"] + "/" + v["filename"]

    # To localize mutable globals, we perform the following steps:
    # 1. Inspect j["call_graph_components"], discarding those which are not all_mutable.
    # 2a. Find the definitions of of each ["mutated_or_escaped_global"].
    # 2b. Construct the transitive closure of all directly used struct/union
    #     definitions needed to define those globals. Note that fields which use
    #     a struct behind a pointer do not require inclusion of that struct definition,
    #     as the type can be forward-declared. (Such type names should be collected in a separate set.)
    # 3. Construct a `struct XjGlobals` declaration containing all mutable globals
    #    adn the struct/union definitions from step 2b.
    #    Place the declaration in a new header file `xj_globals.h`.
    # 4. Based on the definitions from step 2, initialize a singleton XjGlobals instance
    #    in `main()`, called `xjgv`.
    # 5. For each function in the "tissue" part of "mutable_global_tissue", except for `main`,
    #    pass a pointer to the XjGlobals instance, called `xjg`, as an additional first parameter.
    # 6. For each call to a tissue function NOT occurring in `main()`, pass along
    #    the `xjg` parameter.
    # 7. For each call to a tissue function occurring in `main()`, pass `&xjgv`.
    # 8. Each syntatic use of a mutable global named WHATEVER is replaced with `xjg->WHATEVER`.
    # 9. In each file that uses mutable globals, add `#include "xj_globals.h"`
    #
    # Use the `BatchingRewriter` to perform all of these rewrites in a single pass.
    pass

    with batching_rewriter.BatchingRewriter() as rewriter:
        for filepath in compdb.get_source_files():
            rewriter.add_rewrite(
                filepath.as_posix(), 0, 0, "// TODO: implement localization of mutable globals\n"
            )

    print(json.dumps(j, indent=4))

    for filepath in compdb.get_source_files():
        print(f"File: {filepath.as_posix()}")
        print("============================")
        print(filepath.read_text())
        print("============================")


"""
These don't appear to be exposed to IDE completion...

Cursor kinds as of LLVM 18.1.8:
    [
        CursorKind.UNEXPOSED_DECL,
        CursorKind.STRUCT_DECL,
        CursorKind.UNION_DECL,
        CursorKind.CLASS_DECL,
        CursorKind.ENUM_DECL,
        CursorKind.FIELD_DECL,
        CursorKind.ENUM_CONSTANT_DECL,
        CursorKind.FUNCTION_DECL,
        CursorKind.VAR_DECL,
        CursorKind.PARM_DECL,
        CursorKind.OBJC_INTERFACE_DECL,
        CursorKind.OBJC_CATEGORY_DECL,
        CursorKind.OBJC_PROTOCOL_DECL,
        CursorKind.OBJC_PROPERTY_DECL,
        CursorKind.OBJC_IVAR_DECL,
        CursorKind.OBJC_INSTANCE_METHOD_DECL,
        CursorKind.OBJC_CLASS_METHOD_DECL,
        CursorKind.OBJC_IMPLEMENTATION_DECL,
        CursorKind.OBJC_CATEGORY_IMPL_DECL,
        CursorKind.TYPEDEF_DECL,
        CursorKind.CXX_METHOD,
        CursorKind.NAMESPACE,
        CursorKind.LINKAGE_SPEC,
        CursorKind.CONSTRUCTOR,
        CursorKind.DESTRUCTOR,
        CursorKind.CONVERSION_FUNCTION,
        CursorKind.TEMPLATE_TYPE_PARAMETER,
        CursorKind.TEMPLATE_NON_TYPE_PARAMETER,
        CursorKind.TEMPLATE_TEMPLATE_PARAMETER,
        CursorKind.FUNCTION_TEMPLATE,
        CursorKind.CLASS_TEMPLATE,
        CursorKind.CLASS_TEMPLATE_PARTIAL_SPECIALIZATION,
        CursorKind.NAMESPACE_ALIAS,
        CursorKind.USING_DIRECTIVE,
        CursorKind.USING_DECLARATION,
        CursorKind.TYPE_ALIAS_DECL,
        CursorKind.OBJC_SYNTHESIZE_DECL,
        CursorKind.OBJC_DYNAMIC_DECL,
        CursorKind.CXX_ACCESS_SPEC_DECL,
        CursorKind.OBJC_SUPER_CLASS_REF,
        CursorKind.OBJC_PROTOCOL_REF,
        CursorKind.OBJC_CLASS_REF,
        CursorKind.TYPE_REF,
        CursorKind.CXX_BASE_SPECIFIER,
        CursorKind.TEMPLATE_REF,
        CursorKind.NAMESPACE_REF,
        CursorKind.MEMBER_REF,
        CursorKind.LABEL_REF,
        CursorKind.OVERLOADED_DECL_REF,
        CursorKind.VARIABLE_REF,
        CursorKind.INVALID_FILE,
        CursorKind.NO_DECL_FOUND,
        CursorKind.NOT_IMPLEMENTED,
        CursorKind.INVALID_CODE,
        CursorKind.UNEXPOSED_EXPR,
        CursorKind.DECL_REF_EXPR,
        CursorKind.MEMBER_REF_EXPR,
        CursorKind.CALL_EXPR,
        CursorKind.OBJC_MESSAGE_EXPR,
        CursorKind.BLOCK_EXPR,
        CursorKind.INTEGER_LITERAL,
        CursorKind.FLOATING_LITERAL,
        CursorKind.IMAGINARY_LITERAL,
        CursorKind.STRING_LITERAL,
        CursorKind.CHARACTER_LITERAL,
        CursorKind.PAREN_EXPR,
        CursorKind.UNARY_OPERATOR,
        CursorKind.ARRAY_SUBSCRIPT_EXPR,
        CursorKind.BINARY_OPERATOR,
        CursorKind.COMPOUND_ASSIGNMENT_OPERATOR,
        CursorKind.CONDITIONAL_OPERATOR,
        CursorKind.CSTYLE_CAST_EXPR,
        CursorKind.COMPOUND_LITERAL_EXPR,
        CursorKind.INIT_LIST_EXPR,
        CursorKind.ADDR_LABEL_EXPR,
        CursorKind.StmtExpr,
        CursorKind.GENERIC_SELECTION_EXPR,
        CursorKind.GNU_NULL_EXPR,
        CursorKind.CXX_STATIC_CAST_EXPR,
        CursorKind.CXX_DYNAMIC_CAST_EXPR,
        CursorKind.CXX_REINTERPRET_CAST_EXPR,
        CursorKind.CXX_CONST_CAST_EXPR,
        CursorKind.CXX_FUNCTIONAL_CAST_EXPR,
        CursorKind.CXX_TYPEID_EXPR,
        CursorKind.CXX_BOOL_LITERAL_EXPR,
        CursorKind.CXX_NULL_PTR_LITERAL_EXPR,
        CursorKind.CXX_THIS_EXPR,
        CursorKind.CXX_THROW_EXPR,
        CursorKind.CXX_NEW_EXPR,
        CursorKind.CXX_DELETE_EXPR,
        CursorKind.CXX_UNARY_EXPR,
        CursorKind.OBJC_STRING_LITERAL,
        CursorKind.OBJC_ENCODE_EXPR,
        CursorKind.OBJC_SELECTOR_EXPR,
        CursorKind.OBJC_PROTOCOL_EXPR,
        CursorKind.OBJC_BRIDGE_CAST_EXPR,
        CursorKind.PACK_EXPANSION_EXPR,
        CursorKind.SIZE_OF_PACK_EXPR,
        CursorKind.LAMBDA_EXPR,
        CursorKind.OBJ_BOOL_LITERAL_EXPR,
        CursorKind.OBJ_SELF_EXPR,
        CursorKind.OMP_ARRAY_SECTION_EXPR,
        CursorKind.OBJC_AVAILABILITY_CHECK_EXPR,
        CursorKind.UNEXPOSED_STMT,
        CursorKind.LABEL_STMT,
        CursorKind.COMPOUND_STMT,
        CursorKind.CASE_STMT,
        CursorKind.DEFAULT_STMT,
        CursorKind.IF_STMT,
        CursorKind.SWITCH_STMT,
        CursorKind.WHILE_STMT,
        CursorKind.DO_STMT,
        CursorKind.FOR_STMT,
        CursorKind.GOTO_STMT,
        CursorKind.INDIRECT_GOTO_STMT,
        CursorKind.CONTINUE_STMT,
        CursorKind.BREAK_STMT,
        CursorKind.RETURN_STMT,
        CursorKind.ASM_STMT,
        CursorKind.OBJC_AT_TRY_STMT,
        CursorKind.OBJC_AT_CATCH_STMT,
        CursorKind.OBJC_AT_FINALLY_STMT,
        CursorKind.OBJC_AT_THROW_STMT,
        CursorKind.OBJC_AT_SYNCHRONIZED_STMT,
        CursorKind.OBJC_AUTORELEASE_POOL_STMT,
        CursorKind.OBJC_FOR_COLLECTION_STMT,
        CursorKind.CXX_CATCH_STMT,
        CursorKind.CXX_TRY_STMT,
        CursorKind.CXX_FOR_RANGE_STMT,
        CursorKind.SEH_TRY_STMT,
        CursorKind.SEH_EXCEPT_STMT,
        CursorKind.SEH_FINALLY_STMT,
        CursorKind.MS_ASM_STMT,
        CursorKind.NULL_STMT,
        CursorKind.DECL_STMT,
        CursorKind.OMP_PARALLEL_DIRECTIVE,
        CursorKind.OMP_SIMD_DIRECTIVE,
        CursorKind.OMP_FOR_DIRECTIVE,
        CursorKind.OMP_SECTIONS_DIRECTIVE,
        CursorKind.OMP_SECTION_DIRECTIVE,
        CursorKind.OMP_SINGLE_DIRECTIVE,
        CursorKind.OMP_PARALLEL_FOR_DIRECTIVE,
        CursorKind.OMP_PARALLEL_SECTIONS_DIRECTIVE,
        CursorKind.OMP_TASK_DIRECTIVE,
        CursorKind.OMP_MASTER_DIRECTIVE,
        CursorKind.OMP_CRITICAL_DIRECTIVE,
        CursorKind.OMP_TASKYIELD_DIRECTIVE,
        CursorKind.OMP_BARRIER_DIRECTIVE,
        CursorKind.OMP_TASKWAIT_DIRECTIVE,
        CursorKind.OMP_FLUSH_DIRECTIVE,
        CursorKind.SEH_LEAVE_STMT,
        CursorKind.OMP_ORDERED_DIRECTIVE,
        CursorKind.OMP_ATOMIC_DIRECTIVE,
        CursorKind.OMP_FOR_SIMD_DIRECTIVE,
        CursorKind.OMP_PARALLELFORSIMD_DIRECTIVE,
        CursorKind.OMP_TARGET_DIRECTIVE,
        CursorKind.OMP_TEAMS_DIRECTIVE,
        CursorKind.OMP_TASKGROUP_DIRECTIVE,
        CursorKind.OMP_CANCELLATION_POINT_DIRECTIVE,
        CursorKind.OMP_CANCEL_DIRECTIVE,
        CursorKind.OMP_TARGET_DATA_DIRECTIVE,
        CursorKind.OMP_TASK_LOOP_DIRECTIVE,
        CursorKind.OMP_TASK_LOOP_SIMD_DIRECTIVE,
        CursorKind.OMP_DISTRIBUTE_DIRECTIVE,
        CursorKind.OMP_TARGET_ENTER_DATA_DIRECTIVE,
        CursorKind.OMP_TARGET_EXIT_DATA_DIRECTIVE,
        CursorKind.OMP_TARGET_PARALLEL_DIRECTIVE,
        CursorKind.OMP_TARGET_PARALLELFOR_DIRECTIVE,
        CursorKind.OMP_TARGET_UPDATE_DIRECTIVE,
        CursorKind.OMP_DISTRIBUTE_PARALLELFOR_DIRECTIVE,
        CursorKind.OMP_DISTRIBUTE_PARALLEL_FOR_SIMD_DIRECTIVE,
        CursorKind.OMP_DISTRIBUTE_SIMD_DIRECTIVE,
        CursorKind.OMP_TARGET_PARALLEL_FOR_SIMD_DIRECTIVE,
        CursorKind.OMP_TARGET_SIMD_DIRECTIVE,
        CursorKind.OMP_TEAMS_DISTRIBUTE_DIRECTIVE,
        CursorKind.TRANSLATION_UNIT,
        CursorKind.UNEXPOSED_ATTR,
        CursorKind.IB_ACTION_ATTR,
        CursorKind.IB_OUTLET_ATTR,
        CursorKind.IB_OUTLET_COLLECTION_ATTR,
        CursorKind.CXX_FINAL_ATTR,
        CursorKind.CXX_OVERRIDE_ATTR,
        CursorKind.ANNOTATE_ATTR,
        CursorKind.ASM_LABEL_ATTR,
        CursorKind.PACKED_ATTR,
        CursorKind.PURE_ATTR,
        CursorKind.CONST_ATTR,
        CursorKind.NODUPLICATE_ATTR,
        CursorKind.CUDACONSTANT_ATTR,
        CursorKind.CUDADEVICE_ATTR,
        CursorKind.CUDAGLOBAL_ATTR,
        CursorKind.CUDAHOST_ATTR,
        CursorKind.CUDASHARED_ATTR,
        CursorKind.VISIBILITY_ATTR,
        CursorKind.DLLEXPORT_ATTR,
        CursorKind.DLLIMPORT_ATTR,
        CursorKind.CONVERGENT_ATTR,
        CursorKind.WARN_UNUSED_ATTR,
        CursorKind.WARN_UNUSED_RESULT_ATTR,
        CursorKind.ALIGNED_ATTR,
        CursorKind.PREPROCESSING_DIRECTIVE,
        CursorKind.MACRO_DEFINITION,
        CursorKind.MACRO_INSTANTIATION,
        CursorKind.INCLUSION_DIRECTIVE,
        CursorKind.MODULE_IMPORT_DECL,
        CursorKind.TYPE_ALIAS_TEMPLATE_DECL,
        CursorKind.STATIC_ASSERT,
        CursorKind.FRIEND_DECL,
        CursorKind.CONCEPT_DECL,
        CursorKind.OVERLOAD_CANDIDATE,
    ]
    """
