import json
from clang.cindex import (  # type: ignore
    Index,
    CursorKind,
    Config,
    StorageClass,
    TranslationUnit,
    CompilationDatabase,
    TypeKind,
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
from cindex_helpers import render_declaration_sans_qualifiers


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
        tus[abs_path.as_posix()] = tu
    return tus


def preprocess_and_create_new_compdb(
    compdb: compilation_database.CompileCommands,
    target_dir: str,
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

        preprocessed_file_path = (target_dir_path / rel_src_path).with_suffix(".nolines.i")
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

        refold_map_path = preprocessed_file_path.with_suffix(".refoldmap.json")
        subprocess.run(
            [
                *base_pp_command,
                f"--refold-map={refold_map_path.as_posix()}",
                "--no-line-commands",
                "-o",
                str(preprocessed_file_path),
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


def compute_globals_and_statics_for_project(
    compdb: compilation_database.CompileCommands,
    elide_functions: bool = False,
    statics_only: bool = False,
) -> list[Cursor]:
    index = create_xj_clang_index()
    tus = parse_project(index, compdb)
    return compute_globals_and_statics_for_translation_units(
        list(tus.values()), elide_functions, statics_only
    )


def compute_globals_and_statics_for_translation_units(
    translation_units: list[TranslationUnit],
    elide_functions: bool = False,
    statics_only: bool = False,
) -> list[Cursor]:
    combined: list[Cursor] = []
    for tu in translation_units:
        results = compute_globals_and_statics_for_translation_unit(
            tu, elide_functions, statics_only
        )
        combined.extend(results)
    return combined


def mk_NamedDeclInfo(node: Cursor) -> NamedDeclInfo:
    extent = node.extent
    start = extent.start
    end = extent.end

    file_path = start.file.name if start.file else None
    return NamedDeclInfo(
        spelling=node.spelling,
        file_path=file_path,
        decl_start_byte_offset=start.offset,
        decl_end_byte_offset=end.offset,
        start_line=start.line,
        start_col=start.column,
        end_line=end.line,
        end_col=end.column,
    )


def compute_globals_and_statics_for_translation_unit(
    translation_unit: TranslationUnit, elide_functions: bool, statics_only: bool = False
) -> list[Cursor]:
    """Compute globals and static symbols defined in the translation unit."""

    results = []

    def grab(node):
        if elide_functions and node.kind == CursorKind.FUNCTION_DECL:
            return
        results.append(node)

    def visit(node: Cursor):
        if node.kind in (CursorKind.VAR_DECL, CursorKind.FUNCTION_DECL):
            sc = node.storage_class
            # Clang's implementation of `is_definition()` excludes tentative
            # definitions, but we want to include them here.
            is_def_ish = sc != StorageClass.EXTERN and node.linkage != 0
            if is_def_ish:
                top_level_var_decl = node.kind == CursorKind.VAR_DECL and (
                    node.semantic_parent.kind == CursorKind.TRANSLATION_UNIT
                )
                # Include top-level declarations or entities with 'static' storage
                if (top_level_var_decl and not statics_only) or sc == StorageClass.STATIC:
                    grab(node)
        for child in node.get_children():
            visit(child)

    visit(translation_unit.cursor)
    return results


def localize_mutable_globals(
    json_path: Path,
    compdb: compilation_database.CompileCommands,
    prev: Path,
    current_codebase: Path,
):
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
    # 2b. Construct the transitive closure of all directly used struct/union/enum/typedef
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

    # Step 2a: Find definitions of each mutated_or_escaped_global
    index = create_xj_clang_index()
    tus = parse_project(index, compdb)

    # Entries in this list are either plain global names, or for function-scoped statics,
    # they are in the format "function_name.var_name_xjtr_N"
    mangled_mutated_globals = j.get("mutated_or_escaped_global", [])

    def demangle_meg(mangled_name: str) -> str:
        if "." not in mangled_name:
            return mangled_name
        return mangled_name.split(".")[1]

    mutd_or_escd_global_names_list = [demangle_meg(name) for name in mangled_mutated_globals]
    mutd_or_escd_global_names = set(mutd_or_escd_global_names_list)
    assert len(mutd_or_escd_global_names) == len(mutd_or_escd_global_names_list), (
        "Expected all mutated global names to be unique after demangling, "
        + f"but got duplicates within: {mutd_or_escd_global_names_list}"
    )

    print("=" * 80)
    print("STEP 2a: Finding global definitions")
    print("=" * 80)

    globals_and_statics = compute_globals_and_statics_for_translation_units(
        list(tus.values()), elide_functions=True
    )
    mutated_globals_and_statics = [
        c for c in globals_and_statics if c.spelling in mutd_or_escd_global_names
    ]
    # import pprint

    # print("nonvibed globals:")
    # pprint.pprint([mk_NamedDeclInfo(cursor) for cursor in globals_and_statics])

    # Step 2b: Construct transitive closure of struct/union definitions
    print("\n" + "=" * 80)
    print("STEP 2b: Finding struct/union dependencies")
    print("=" * 80)

    needed_struct_defs = {}
    needed_typedefs: dict[str, tuple[Cursor, str]] = {}
    forward_declarable_types = set()

    def collect_type_dependencies(type_obj_noncanonical, depth=0):
        """Recursively collect struct/union types needed to define this type."""
        indent = "  " * depth

        # Get the canonical type
        type_obj_canonical = type_obj_noncanonical.get_canonical()

        print(
            f"{indent}Analyzing type: {type_obj_noncanonical.spelling} (kind: {type_obj_noncanonical.kind}) "
            + f" [canon spelling: {type_obj_canonical.spelling}]"
            + f" @canon {type_obj_canonical.get_declaration().location}"
            + f" @noncanon {type_obj_noncanonical.get_declaration().location}"
        )

        # Handle occurrences of typedef'ed names
        if type_obj_noncanonical.kind == TypeKind.ELABORATED:
            decl = type_obj_noncanonical.get_declaration()
            type_name = decl.spelling
            assert type_name, "Typedef without a name?"
            if type_name not in needed_typedefs:
                print(f"{indent}  -> Need typedef: {type_name}")
                needed_typedefs[type_name] = (
                    decl,
                    decl.underlying_typedef_type.get_canonical().spelling,
                )
            elif needed_typedefs[type_name][0] == decl:
                pass
            elif (
                needed_typedefs[type_name][1]
                == decl.underlying_typedef_type.get_canonical().spelling
            ):
                # Typedefs in preprocessed code can be duplicated between translation units,
                # as long as the associated types are identical, it's all good.
                pass
            elif needed_typedefs[type_name][0] != decl:
                raise ValueError(
                    f"{indent}  -> Typedef {type_name} already recorded, but different declaration!"
                )

            # Continue with the underlying type
            typedef_decl = decl.get_definition().referenced
            print(f"{indent}  Saw typedef elaborated...")
            print(f"{indent}    typedef cursor: {typedef_decl.kind}")
            print(f"{indent}    typedef cursor: {typedef_decl.extent}")
            print(f"{indent}    typedef type: {typedef_decl.type}")
            print(f"{indent}    typedef type: {typedef_decl.type.kind}")
            collect_type_dependencies(typedef_decl.type, depth + 1)
            return

        if type_obj_noncanonical.kind == TypeKind.TYPEDEF:
            typedef_decl = type_obj_noncanonical.get_declaration()
            print(f"{indent}  Saw typedef...")
            print(f"{indent}    typedef cursor: {typedef_decl.kind}")
            print(f"{indent}    typedef cursor: {typedef_decl.extent}")
            print(f"{indent}    typedef type: {typedef_decl.type}")
            print(f"{indent}    typedef type: {typedef_decl.type.kind}")
            print(f"{indent}    underlying type: {typedef_decl.underlying_typedef_type.kind}")
            print(f"{indent}    referenced type: {typedef_decl.get_definition().referenced.kind}")
            collect_type_dependencies(typedef_decl.underlying_typedef_type, depth + 1)
            pass

        if type_obj_canonical.kind == TypeKind.POINTER:
            assert type_obj_noncanonical.kind == TypeKind.POINTER

        # If it's a pointer, the pointee can be forward-declared
        if type_obj_noncanonical.kind == TypeKind.POINTER:
            pointee = type_obj_noncanonical.get_pointee()
            pointee_canonical = pointee.get_canonical()
            print(f"{indent}  Pointer to: {pointee.spelling}")

            # Check if pointee is a struct/union
            decl = pointee_canonical.get_declaration()
            if decl.kind in [CursorKind.STRUCT_DECL, CursorKind.UNION_DECL]:
                forward_declarable_types.add(decl.spelling)
                print(f"{indent}  -> Can forward-declare: {decl.spelling}")
            return

        if type_obj_canonical.kind in (
            TypeKind.CONSTANTARRAY,
            TypeKind.INCOMPLETEARRAY,
            TypeKind.VARIABLEARRAY,
        ):
            assert type_obj_noncanonical.kind == type_obj_canonical.kind

            elem_type = type_obj_noncanonical.get_array_element_type()
            print(f"{indent}  Array of: {elem_type.spelling}")
            collect_type_dependencies(elem_type, depth + 1)
            return

        # If it's a struct or union, we need its full definition
        decl = type_obj_noncanonical.get_declaration()
        canonical_decl = type_obj_canonical.get_declaration()

        if canonical_decl.kind in [CursorKind.STRUCT_DECL, CursorKind.UNION_DECL]:
            assert canonical_decl.kind == decl.kind, (
                f"Mismatched declaration kinds: {canonical_decl.kind} vs {decl.kind}"
            )
            type_name = decl.spelling
            if type_name and type_name not in needed_struct_defs:
                print(f"{indent}  -> Need full definition: {type_name}")
                needed_struct_defs[type_name] = decl

                # Recursively process fields
                for field in decl.get_children():
                    if field.kind == CursorKind.FIELD_DECL:
                        print(f"{indent}    Field: {field.spelling} : {field.type.spelling}")
                        collect_type_dependencies(field.type, depth + 2)

    for cursor in mutated_globals_and_statics:
        print(f"\nAnalyzing dependencies for {cursor.spelling}:")
        collect_type_dependencies(cursor.type, depth=1)

    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"\nFound {len(mutated_globals_and_statics)} mutated global definitions:")
    for cursor in mutated_globals_and_statics:
        print(
            f"  - {cursor.spelling}: {cursor.type.spelling} at {cursor.location.file}:{cursor.location.line}"
        )

    print(f"\nNeed {len(needed_struct_defs)} struct/union definitions:")
    for name, decl in needed_struct_defs.items():
        print(f"  - {name} -> {decl.location}")
        print(f"           -> {decl.extent}")
        print(f"           -> {decl.type}")
        print(f"           -> {decl.type.spelling}")
        print(f"           -> {decl.get_usr()}")
        print(f"           -> {decl.get_definition()}")
        print(f"           -> {decl.get_definition().extent}")
        print()

    print(f"\nNeed {len(needed_typedefs)} typedef definitions:")
    for name, (decl, canonical_spelling) in needed_typedefs.items():
        print(f"  - {name} -> {decl.location}")
        print(f"           -> {decl.extent}")
        print(f"           -> {decl.type}")
        print(f"           -> {decl.type.spelling}")
        print(f"  canon_ty -> {canonical_spelling}")
        print(f"           -> {decl.get_usr()}")
        print(f"           -> {decl.get_definition()}")
        print(f"           -> {decl.get_definition().extent}")
        print()

    print(f"\nCan forward-declare {len(forward_declarable_types)} types:")
    for name in forward_declarable_types:
        print(f"  - {name}")
    print("=" * 80)

    # Steps 5 and 6: Modify function signatures and call sites
    print("\n" + "=" * 80)
    print("STEPS 5 & 6: Modifying function signatures and call sites")
    print("=" * 80)

    tissue_functions = set(j.get("mutable_global_tissue", {}).get("tissue", []))
    tissue_functions.discard("main")  # Don't modify main

    print(f"\nTissue functions to modify: {tissue_functions}")

    # Collect all function definitions and call sites from JSON
    function_defs = {}  # function_name -> {cursor, file, abs_path}
    call_sites_from_json = []  # From the JSON analysis

    # Get call sites from JSON (more reliable than libclang semantic_parent)
    for component in j.get("call_graph_components", []):
        if not component.get("all_mutable", False):
            continue
        call_targets = component.get("call_targets", [])
        for target in call_targets:
            # Target format: "<llvm-link>:function_name"
            if ":" in target:
                callee_func = target.split(":")[-1]
                if callee_func in tissue_functions:
                    # Record all call sites for this target
                    for site in component.get("call_sites", []):
                        caller_func = site.get("p")
                        uf = site.get("uf")
                        line = site.get("line")
                        col = site.get("col")
                        call_sites_from_json.append({
                            "caller_func": caller_func,
                            "callee_func": callee_func,
                            "uf": uf,
                            "line": line,
                            "col": col,
                        })
                        print(
                            f"  From JSON: {caller_func} calls {callee_func} at {uf}:{line}:{col}"
                        )

    for abs_path, tu in tus.items():
        for cursor in tu.cursor.walk_preorder():
            # Find function definitions
            if cursor.kind == CursorKind.FUNCTION_DECL and cursor.is_definition():
                func_name = cursor.spelling
                if func_name in tissue_functions:
                    function_defs[func_name] = {
                        "cursor": cursor,
                        "file": abs_path,
                        "extent": cursor.extent,
                    }
                    print(
                        f"\nFound function definition: {func_name} at {abs_path}:{cursor.location.line}"
                    )

    # Apply all rewrites using a single BatchingRewriter
    # This ensures offsets are calculated correctly
    with batching_rewriter.BatchingRewriter() as rewriter:
        # Step 5: Add xjg parameter to tissue function signatures (definitions and declarations)

        # Collect all declarations (both definitions and forward declarations)
        all_function_cursors: dict[
            str, list[dict[str, Cursor | str | bool]]
        ] = {}  # func_name -> list of (cursor, file, is_definition)

        for abs_path, tu in tus.items():
            for cursor in tu.cursor.walk_preorder():
                if cursor.kind == CursorKind.FUNCTION_DECL:
                    func_name = cursor.spelling
                    if func_name in tissue_functions:
                        if func_name not in all_function_cursors:
                            all_function_cursors[func_name] = []
                        all_function_cursors[func_name].append({
                            "cursor": cursor,
                            "file": abs_path,
                            "is_definition": cursor.is_definition(),
                        })

        # Update all function signatures (both declarations and definitions)
        for func_name, cursors_list in all_function_cursors.items():
            for func_info in cursors_list:
                cursor = func_info["cursor"]
                file_path = func_info["file"]
                is_def = func_info["is_definition"]

                # Find the position after the opening parenthesis
                with open(file_path, "rb") as f:
                    content = f.read()

                # Get the function start location
                func_start_offset = cursor.extent.start.offset

                # Find the opening parenthesis
                paren_pos = content.find(b"(", func_start_offset)
                if paren_pos == -1:
                    continue

                # Check if there are existing parameters
                closing_paren_pos = content.find(b")", paren_pos)
                if closing_paren_pos == -1:
                    continue

                # Check if there are parameters already
                param_section = content[paren_pos + 1 : closing_paren_pos].strip()

                decl_type = "definition" if is_def else "declaration"

                if param_section == b"" or param_section == b"void":
                    # No parameters, just add our parameter
                    insert_offset = paren_pos + 1
                    insert_text = "struct XjGlobals *xjg"
                    print(f"  Adding xjg parameter to {func_name} {decl_type} (no existing params)")
                else:
                    # Has parameters, add as first parameter with comma
                    insert_offset = paren_pos + 1
                    insert_text = "struct XjGlobals *xjg, "
                    print(
                        f"  Adding xjg parameter to {func_name} {decl_type} (with existing params)"
                    )

                rewriter.add_rewrite(file_path, insert_offset, 0, insert_text)

        # Step 6: Modify call sites to pass xjg (using JSON call site info)
        for call_info in call_sites_from_json:
            caller_func = call_info["caller_func"]
            callee_func = call_info["callee_func"]
            uf = call_info["uf"]
            line = call_info["line"]
            col = call_info["col"]

            # Get the actual file path - need to adjust for current directory
            # The JSON has paths from c_03 but we're working in c_04
            file_path_old = un_uf(uf)
            assert file_path_old.startswith(prev.as_posix())
            i_file_path = file_path_old.replace(prev.as_posix(), current_codebase.as_posix())

            # Working with .i files (in particular, ones without line markers)
            # allows us to reliably edit call sites. Otherwise, we'd have to contend
            # with call sites that are synthesized by the preprocessor in horrific ways.
            assert i_file_path.endswith(".nolines.i")
            assert i_file_path in tus

            # Determine what to pass based on caller
            if caller_func == "main":
                param_to_pass = "&xjgv"
            elif caller_func in tissue_functions:
                param_to_pass = "xjg"
            else:
                # Caller is not in tissue, skip for now
                print(f"    Skipping: caller {caller_func} not in tissue")
                continue

            # Find the call expression at the given location
            # We need to use libclang to find the exact offset
            found_call = False
            tu = tus[i_file_path]
            for cursor in tu.cursor.walk_preorder():
                if cursor.kind != CursorKind.CALL_EXPR:
                    continue

                loc = cursor.location
                if line != loc.line or col != loc.column:
                    continue

                if loc.line == line and loc.column == col and str(loc.file) == i_file_path:
                    # Found the call
                    call_start_offset = cursor.extent.start.offset

                    # Read file to find parenthesis
                    with open(i_file_path, "rb") as f:
                        content = f.read()

                    paren_pos = content.find(b"(", call_start_offset)
                    if paren_pos == -1:
                        break

                    # Check if there are existing arguments
                    closing_paren_pos = content.find(b")", paren_pos)
                    if closing_paren_pos == -1:
                        break

                    args_section = content[paren_pos + 1 : closing_paren_pos].strip()

                    if args_section == b"":
                        # No arguments
                        insert_offset = paren_pos + 1
                        insert_text = param_to_pass
                        print(
                            f"  Passing {param_to_pass} to {callee_func} from {caller_func} at {uf}:{line} (no args)"
                        )
                    else:
                        # Has arguments, add as first argument with comma
                        insert_offset = paren_pos + 1
                        insert_text = param_to_pass + ", "
                        print(
                            f"  Passing {param_to_pass} to {callee_func} from {caller_func} at {uf}:{line} (with args)"
                        )

                    rewriter.add_rewrite(i_file_path, insert_offset, 0, insert_text)
                    found_call = True
                    break

            if not found_call:
                raise ValueError(
                    f"  WARNING: Could not find call to {callee_func} from {caller_func} at {uf}:{line}:{col}"
                )

        # Step 8: Replace uses of mutable globals with xjg->WHATEVER
        print("\n  --- Step 8: Replacing global variable accesses ---")
        for abs_path, tu in tus.items():
            for cursor in tu.cursor.walk_preorder():
                if cursor.kind == CursorKind.FUNCTION_DECL:
                    for child in cursor.walk_preorder():
                        if (
                            child.kind == CursorKind.DECL_REF_EXPR
                            and child.spelling in mutd_or_escd_global_names
                        ):
                            # Get the extent of the variable reference
                            start_offset = child.extent.start.offset
                            end_offset = child.extent.end.offset
                            length = end_offset - start_offset
                            var_name = child.spelling

                            replacement = f"xjg->{var_name}"
                            print(
                                f"    Found DECL_REF_EXPR for {var_name} at {abs_path}:{child.location.line}:{child.location.column}"
                            )

                            # Check the parent - if it's a VAR_DECL, skip it
                            parent = child.semantic_parent
                            if (
                                parent
                                and parent.kind == CursorKind.VAR_DECL
                                and parent.spelling == var_name
                            ):
                                print("      Skipping: this is part of the declaration")
                                continue

                            print(f"    Replacing {var_name} with {replacement}")

                            rewriter.add_rewrite(abs_path, start_offset, length, replacement)
                    break

        # Step 3: Create xj_globals_fwd.h and xj_globals.h header files
        print("\n  --- Step 3: Creating xj_globals_fwd.h and xj_globals.h ---")

        # Get the directory where we should place the headers
        # Use the directory from the compilation database
        if compdb.get_source_files():
            header_dir = compdb.get_source_files()[0].parent
        else:
            header_dir = Path(".")

        # Create xj_globals_fwd.h with forward declarations only
        fwd_header_path = header_dir / "xj_globals_fwd.h"
        print(f"  Creating forward declaration header at {fwd_header_path}")

        fwd_header_lines = []
        fwd_header_lines.append("#ifndef XJ_GLOBALS_FWD_H")
        fwd_header_lines.append("#define XJ_GLOBALS_FWD_H")
        fwd_header_lines.append("")
        fwd_header_lines.append("struct XjGlobals;")
        fwd_header_lines.append("")
        fwd_header_lines.append("#endif /* XJ_GLOBALS_FWD_H */")
        fwd_header_lines.append("")

        with open(fwd_header_path, "w", encoding="utf-8") as fh:
            fh.write("\n".join(fwd_header_lines))

        print(f"  Created forward declaration header")

        # Create xj_globals.h with full definitions
        header_path = header_dir / "xj_globals.h"
        print(f"  Creating full definition header at {header_path}")

        # Build the header content
        header_lines = []
        header_lines.append("#ifndef XJ_GLOBALS_H")
        header_lines.append("#define XJ_GLOBALS_H")
        header_lines.append("")

        # Add forward declarations if needed
        if forward_declarable_types:
            for type_name in sorted(forward_declarable_types):
                # Determine if it's a struct or union
                # We'll default to struct (can improve this later)
                header_lines.append(f"struct {type_name};")
            header_lines.append("")

        # Add typedefs
        typedefs_sorted_by_line = sorted(
            list(needed_typedefs.items()), key=lambda item: item[1][0].location.line
        )
        if typedefs_sorted_by_line:
            for name, (decl_cursor, _u_t_canonical_spelling) in typedefs_sorted_by_line:
                # Get the full definition text
                start_offset = decl_cursor.extent.start.offset
                end_offset = decl_cursor.extent.end.offset
                # Find the file containing this definition
                for abs_path, tu in tus.items():
                    if str(abs_path) == decl_cursor.location.file.name:
                        with open(abs_path, "rb") as f:
                            content = f.read()
                        typedef_text = content[start_offset:end_offset].decode("utf-8")
                        header_lines.append(typedef_text + ";")
                        break

            header_lines.append("")

        # Add full struct/union definitions from step 2b
        if needed_struct_defs:
            for type_name, decl_cursor in needed_struct_defs.items():
                # Get the full definition text
                # We need to extract the source text for this struct/union
                start_offset = decl_cursor.extent.start.offset
                end_offset = decl_cursor.extent.end.offset

                # Find the file containing this definition
                for abs_path, tu in tus.items():
                    if str(abs_path) == decl_cursor.location.file.name:
                        with open(abs_path, "rb") as f:
                            content = f.read()
                        struct_text = content[start_offset:end_offset].decode("utf-8")
                        header_lines.append(struct_text + ";")
                        header_lines.append("")
                        break

        # Add the XjGlobals struct definition
        mutated_globals_cursors_by_name = {c.spelling: c for c in mutated_globals_and_statics}
        assert len(mutated_globals_cursors_by_name) == len(mutated_globals_and_statics), (
            "Expected all mutated global names to be unique, "
            + f"but got duplicates within: {mutated_globals_cursors_by_name.keys()}"
        )

        header_lines.append("struct XjGlobals {")
        for global_name in sorted(mutated_globals_cursors_by_name.keys()):
            var_cursor = mutated_globals_cursors_by_name[global_name]
            var_name = var_cursor.spelling
            type_spelling = var_cursor.type.spelling

            # Get the initializer if present
            initializer = None
            for child in var_cursor.get_children():
                # The initializer is a child of the VAR_DECL
                if child.kind != CursorKind.TYPE_REF:
                    # This is likely the initializer
                    # Extract its text
                    init_start = child.extent.start.offset
                    init_end = child.extent.end.offset
                    file_path = var_cursor.location.file.name
                    with open(file_path, "rb") as f:
                        content = f.read()
                    initializer = content[init_start:init_end].decode("utf-8").strip()
                    break

            # Add the field (we'll handle initialization separately)
            header_lines.append(render_declaration_sans_qualifiers(var_cursor.type, var_name) + ";")

        header_lines.append("};")
        header_lines.append("")
        header_lines.append("#endif /* XJ_GLOBALS_H */")
        header_lines.append("")

        # Write the header file
        with open(header_path, "w", encoding="utf-8") as fh:
            fh.write("\n".join(header_lines))

        # Step 4: Initialize xjgv in main()
        print("\n  --- Step 4: Initializing xjgv in main() ---")

        # First, we need to detect which globals reference other globals
        # and build a dependency graph
        print("\n  Analyzing global dependencies for initializers...")

        global_dependencies: dict[str, set[str]] = {}  # global_name -> set of referenced globals

        for var_cursor in mutated_globals_and_statics:
            dependencies = set()

            # Walk through the initializer expression to find DECL_REF_EXPR nodes
            for child in var_cursor.walk_preorder():
                if (
                    child.kind == CursorKind.DECL_REF_EXPR
                    and child.spelling not in mutd_or_escd_global_names
                ):
                    dependencies.add(child.spelling)
                    print(f"    {var_cursor.spelling} references {child.spelling}")

            global_dependencies[var_cursor.spelling] = dependencies

        # Compute transitive closure of dependencies for all globals
        # We need to copy (not move) any global that is referenced by another
        globals_to_copy_to_main = set()

        def collect_transitive_deps(global_name: str, visited: set[str]) -> None:
            if global_name in visited:
                return
            visited.add(global_name)
            for dep in global_dependencies.get(global_name, set()):
                globals_to_copy_to_main.add(dep)
                collect_transitive_deps(dep, visited)

        for global_name in mutd_or_escd_global_names_list:
            collect_transitive_deps(global_name, set())

        print(f"\n  Globals to copy into main before xjgv: {globals_to_copy_to_main}")

        # Find main() function and insert initialization at the beginning
        for abs_path, tu in tus.items():
            for cursor in tu.cursor.walk_preorder():
                if (
                    cursor.kind == CursorKind.FUNCTION_DECL
                    and cursor.spelling == "main"
                    and cursor.is_definition()
                ):
                    print(f"  Found main() at {abs_path}:{cursor.location.line}")

                    globals_and_statics_by_name = {c.spelling: c for c in globals_and_statics}

                    # Find the opening brace of main's body
                    # The compound statement is a child of the function
                    for child in cursor.get_children():
                        if child.kind == CursorKind.COMPOUND_STMT:
                            # Insert after the opening brace
                            insert_offset = child.extent.start.offset + 1

                            # Build initialization code
                            init_lines = []

                            # First, copy definitions of referenced globals
                            if globals_to_copy_to_main:
                                init_lines.append("")
                                init_lines.append(
                                    "// Local copies of globals referenced by other globals"
                                )

                                # Sort by dependency order (topological sort)
                                sorted_globals = []
                                visited = set()

                                def visit_for_topo(g: str) -> None:
                                    if g in visited or g not in globals_to_copy_to_main:
                                        return
                                    visited.add(g)
                                    for dep in global_dependencies.get(g, set()):
                                        if dep in globals_to_copy_to_main:
                                            visit_for_topo(dep)
                                    sorted_globals.append(g)

                                for g in globals_to_copy_to_main:
                                    visit_for_topo(g)

                                # Generate local variable definitions
                                for global_name in sorted_globals:
                                    var_cursor = globals_and_statics_by_name.get(global_name)
                                    if var_cursor is None:
                                        # We want to copy immutable globals, but function that
                                        # escape are not in the globals_and_statics list, and
                                        # we don't want to (& cannot) duplicate their definitions
                                        # within main().
                                        continue
                                    var_name = var_cursor.spelling
                                    type_spelling = var_cursor.type.spelling

                                    # Get the initializer value
                                    initializer = "0"  # Default
                                    for child_node in var_cursor.get_children():
                                        if child_node.kind != CursorKind.TYPE_REF:
                                            init_start = child_node.extent.start.offset
                                            init_end = child_node.extent.end.offset
                                            with open(var_cursor.location.file.name, "rb") as f:
                                                content = f.read()
                                            initializer = (
                                                content[init_start:init_end].decode("utf-8").strip()
                                            )
                                            break

                                    init_lines.append(
                                        f"  static {type_spelling} {var_name} = {initializer};"
                                    )

                                init_lines.append("")

                            init_lines.append("\n  struct XjGlobals xjgv = {")

                            # Initialize each field based on original initializers
                            field_inits = []
                            for global_name in sorted(mutated_globals_cursors_by_name.keys()):
                                var_cursor = mutated_globals_cursors_by_name[global_name]
                                initializer = "0"  # Default
                                for child_node in var_cursor.get_children():
                                    if child_node.kind != CursorKind.TYPE_REF:
                                        init_start = child_node.extent.start.offset
                                        init_end = child_node.extent.end.offset
                                        with open(var_cursor.location.file.name, "rb") as f:
                                            content = f.read()
                                        initializer = (
                                            content[init_start:init_end].decode("utf-8").strip()
                                        )
                                        break

                                field_inits.append(f"    .{global_name} = {initializer}")

                            init_lines.append(",\n".join(field_inits))
                            init_lines.append("\n  };")

                            init_text = "\n".join(init_lines)
                            rewriter.add_rewrite(abs_path, insert_offset, 0, init_text)
                            print("  Added xjgv initialization in main()")
                            break
                    break

        # Step 9: Add includes and type definitions to files that use mutable globals
        print("\n  --- Step 9: Adding includes and type definitions ---")

        # Find the file containing main() and its main function cursor
        main_file = None
        main_cursor = None
        for abs_path, tu in tus.items():
            for cursor in tu.cursor.walk_preorder():
                if cursor.kind == CursorKind.FUNCTION_DECL and cursor.spelling == "main":
                    main_file = abs_path
                    main_cursor = cursor
                    break
            if main_file:
                break

        # Collect files that need includes
        files_needing_include = set()

        # Files with global definitions (not including main file)
        for cursor in mutated_globals_and_statics:
            if cursor.location.file.name != main_file:
                files_needing_include.add(cursor.location.file.name)

        # Add forward declaration includes to non-main files
        for file_path_str in files_needing_include:
            include_text = '#include "xj_globals_fwd.h"\n'
            print(f"  Adding xj_globals_fwd.h to {file_path_str}")
            rewriter.add_rewrite(file_path_str, 0, 0, include_text)

        # For the main file, collect types already in scope
        if main_file and main_cursor:
            print(f"\n  Analyzing types in scope in main TU: {main_file}")

            types_in_scope = set()  # Set of type names (struct/union/typedef)

            # Walk the main TU to find all struct/union/typedef declarations
            main_tu = tus[main_file]
            for cursor in main_tu.cursor.walk_preorder():
                if cursor.kind == CursorKind.STRUCT_DECL and cursor.spelling:
                    types_in_scope.add(cursor.spelling)
                    print(f"    Found struct in scope: {cursor.spelling}")
                elif cursor.kind == CursorKind.UNION_DECL and cursor.spelling:
                    types_in_scope.add(cursor.spelling)
                    print(f"    Found union in scope: {cursor.spelling}")
                elif cursor.kind == CursorKind.TYPEDEF_DECL and cursor.spelling:
                    types_in_scope.add(cursor.spelling)
                    print(f"    Found typedef in scope: {cursor.spelling}")

            print(f"\n  Found {len(types_in_scope)} types already in scope in main TU")

            # Determine which types need to be emitted
            types_to_emit_structs = {}  # name -> decl_cursor
            types_to_emit_typedefs = {}  # name -> decl_cursor

            for type_name, decl_cursor in needed_struct_defs.items():
                if type_name not in types_in_scope:
                    types_to_emit_structs[type_name] = decl_cursor
                    print(f"    Will emit struct definition: {type_name}")
                else:
                    print(f"    Skipping struct (already in scope): {type_name}")

            for type_name, decl_cursor in needed_typedefs.items():
                if type_name not in types_in_scope:
                    types_to_emit_typedefs[type_name] = decl_cursor
                    print(f"    Will emit typedef: {type_name}")
                else:
                    print(f"    Skipping typedef (already in scope): {type_name}")

            # Build type definitions to insert before main()
            type_defs_lines = []
            type_defs_lines.append("\n// Type definitions needed for XjGlobals")

            # Add forward declarations if needed
            forward_decls_to_emit = forward_declarable_types - types_in_scope
            if forward_decls_to_emit:
                for type_name in sorted(forward_decls_to_emit):
                    type_defs_lines.append(f"struct {type_name};")

            # Add typedefs
            if types_to_emit_typedefs:
                typedefs_sorted = sorted(
                    list(types_to_emit_typedefs.items()), key=lambda item: item[1].location.line
                )
                for name, decl_cursor in typedefs_sorted:
                    start_offset = decl_cursor.extent.start.offset
                    end_offset = decl_cursor.extent.end.offset
                    for abs_path, tu in tus.items():
                        if str(abs_path) == decl_cursor.location.file.name:
                            with open(abs_path, "rb") as f:
                                content = f.read()
                            typedef_text = content[start_offset:end_offset].decode("utf-8")
                            type_defs_lines.append(typedef_text + ";")
                            break

            # Add struct/union definitions
            if types_to_emit_structs:
                for type_name, decl_cursor in types_to_emit_structs.items():
                    start_offset = decl_cursor.extent.start.offset
                    end_offset = decl_cursor.extent.end.offset
                    for abs_path, tu in tus.items():
                        if str(abs_path) == decl_cursor.location.file.name:
                            with open(abs_path, "rb") as f:
                                content = f.read()
                            struct_text = content[start_offset:end_offset].decode("utf-8")
                            type_defs_lines.append(struct_text + ";")
                            break

            # Add XjGlobals struct definition
            type_defs_lines.append("\nstruct XjGlobals {")
            for global_name in sorted(mutated_globals_cursors_by_name.keys()):
                var_cursor = mutated_globals_cursors_by_name[global_name]
                type_defs_lines.append(
                    render_declaration_sans_qualifiers(var_cursor.type, var_cursor.spelling) + ";"
                )

            type_defs_lines.append("};")
            type_defs_lines.append("")

            # Insert before main() function
            insert_offset = main_cursor.extent.start.offset
            type_defs_text = "\n".join(type_defs_lines) + "\n"
            rewriter.add_rewrite(main_file, insert_offset, 0, type_defs_text)
            print(f"\n  Added type definitions before main() in {main_file}")

    print("=" * 80)


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
