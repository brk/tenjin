import json
import shutil
import time
import tempfile
from pathlib import Path
from typing import Callable
from subprocess import CompletedProcess
import hashlib
from dataclasses import dataclass

import compilation_database
import c_refact
import c_refact_arglifter
import c_refact_type_mod_replicator
import hermetic
import repo_root
import ingest_tracking
import llvm_bitcode_linking
from caching_file_contents import FilePathStr, CachingFileContents
from constants import WANT, XJ_GUIDANCE_FILENAME


def elapsed_ms_of_ns(start_ns: int, end_ns: int) -> float:
    """Calculate elapsed time in milliseconds from nanoseconds."""
    return (end_ns - start_ns) / 1_000_000.0


def materialize_compilation_database_in(
    builddir: Path, codebase: Path, buildcmd: str | None, tracker: ingest_tracking.TimingRepo
):
    """Leaves a copy of a provided-or-generated compile_commands.json file
    in the given build directory.

    The compile_commands.json file will refer to paths in the original codebase,
    not the build directory.

    The build directory may or may not end up with a copy of the input codebase,
    and in-place build artifacts,
    depending on whether a build was required to produce the compilation database."""
    provided_compdb = codebase / "compile_commands.json"
    provided_cmakelists = codebase / "CMakeLists.txt"

    if provided_cmakelists.exists() and not provided_compdb.exists():
        # If we have a CMakeLists.txt, we can generate the compile_commands.json
        cp = hermetic.run(
            [
                "cmake",
                "-S",
                str(codebase),
                "-B",
                str(builddir),
                "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
            ],
            check=True,
            capture_output=True,
        )
        tracker.update_sub(cp)
    elif codebase.is_file() and codebase.suffix == ".c":
        # If we have a single C file, we can trivially generate a compile_commands.json
        # with a single entry for it.
        compilation_database.write_synthetic_compile_commands_to(
            builddir / "compile_commands.json", codebase, codebase.parent
        )
    elif buildcmd:
        # If we have a build command, use it to generate a compile_commands.json file
        # by invoking the build command from a temporary directory with a copy of the
        # input codebase.
        shutil.copytree(codebase, builddir, dirs_exist_ok=True)
        hermetic.run(f"intercept-build {buildcmd}", cwd=builddir, shell=True, check=True)
        # intercept-build will have generated this file, if all went well
        extracted_compdb = builddir / "compile_commands.json"
        extracted_compdb_bytes = extracted_compdb.read_bytes()
        if extracted_compdb_bytes == b"[]":
            # Perhaps the build command failed, or it cut off early,
            # for example if the target binary already existed.
            raise ValueError("Extracted compile_commands.json is empty")
        else:
            compilation_database.rebase_compile_commands_from_to(
                extracted_compdb, builddir, codebase
            )

    elif provided_compdb.exists():
        # Otherwise, we assume the compile_commands.json is already present.
        # We must make a copy to freely munge without affecting the original.
        shutil.copyfile(provided_compdb, builddir / "compile_commands.json")
    else:
        # If the codebase is a directory containing a single C file,
        # we assume it's the intended source file to compile.
        c_files = list(codebase.glob("*.c"))
        if len(c_files) == 1:
            compilation_database.write_synthetic_compile_commands_to(
                builddir / "compile_commands.json", c_files[0], codebase
            )
        else:
            raise FileNotFoundError(
                f"No compile_commands.json found in {codebase}, "
                "and unable to generate one automatically."
            )


def compdb_path_in(dir: Path) -> Path:
    return dir / "compile_commands.json"


def copy_codebase(
    src: Path,
    dst: Path,
):
    """Copy the original codebase (file or directory) to a new directory."""
    assert src.is_dir()
    shutil.copytree(src, dst)
    if compdb_path_in(dst).exists():
        compilation_database.rebase_compile_commands_from_to(compdb_path_in(dst), src, dst)

        # print("Rebased compile_commands.json")
        # print("from ", src)
        # print("to   ", dst)
        # print(compdb_path_in(dst).read_text())


type QUSS = str
type FileContentsStr = str


def collect_decls_by_tu(
    current_codebase: Path,
    restricted_to_files: set[FilePathStr] | None = None,
) -> dict[FilePathStr, dict[QUSS, tuple[FilePathStr, int, int, FileContentsStr]]]:
    def path_of_interest(p: FilePathStr) -> bool:
        return restricted_to_files is None or p in restricted_to_files

    compdb = compilation_database.CompileCommands.from_json_file(compdb_path_in(current_codebase))

    header_contents = CachingFileContents()

    decls_by_tu: dict[FilePathStr, dict[QUSS, tuple[FilePathStr, int, int, FileContentsStr]]] = {}

    # Note that the two FilePathStrs here can be different,
    # e.g. the declaration can be in a header file included by the TU file.

    index = c_refact.create_xj_clang_index()
    tus = c_refact.parse_project(index, compdb)
    for tu_path, tu in tus.items():
        for cursor in tu.cursor.get_children():
            # When we run this pass before expanding the preprocessor,
            # cursor.location can reflect header file locations.
            if (
                cursor.kind.is_declaration()
                and cursor.location.file
                and path_of_interest(cursor.location.file.name)
            ):
                q = c_refact_type_mod_replicator.quss(cursor, None)
                # print("Recording declaration:", cursor.spelling)
                # print("   Kind:", cursor.kind)
                # print("   QUSS:", q)
                # print("   From location:", cursor.location.file.name)
                # print("   found in TU:", tu_path)
                decls_by_tu.setdefault(tu_path, {})[q] = (
                    cursor.location.file.name,
                    cursor.extent.start.offset,
                    cursor.extent.end.offset,
                    header_contents.get_bytes(cursor.location.file.name)[
                        cursor.extent.start.offset : cursor.extent.end.offset
                    ].decode("utf-8"),
                )
    return decls_by_tu


def organize_decls_by_headers(
    decls_by_tu: dict[FilePathStr, dict[QUSS, tuple[FilePathStr, int, int, FileContentsStr]]],
) -> dict[FilePathStr, dict[QUSS, tuple[int, int, FileContentsStr]]]:
    # decls_by_tu is indexed by TU path, and can contain entry[0] values that are not TU paths,
    # e.g. header file paths.
    #
    # We want to construct a mapping indexed by header file path,
    # containing entries for declarations that are consistent across all TUs
    # (in which those decls appear, at least).
    quss_in_every_tu = set()
    for tu_decls in decls_by_tu.values():
        tu_quss = set(tu_decls.keys())
        if not quss_in_every_tu:
            quss_in_every_tu = tu_quss
        else:
            quss_in_every_tu = quss_in_every_tu.intersection(tu_quss)

    tu_paths = list(decls_by_tu.keys())
    assert len(tu_paths) > 0, "If there are no TUs, there can't be any common decls"

    decls_by_header: dict[FilePathStr, dict[QUSS, tuple[int, int, FileContentsStr]]] = {}
    for q in quss_in_every_tu:
        entries_dups = [decls_by_tu[tu_path][q] for tu_path in tu_paths]
        entries_set = set(entries_dups)
        if len(entries_set) > 1:
            if len(set(e[0] for e in entries_dups)) > 1:
                print("Declaration locations differ between TUs for QUSS:", q)
            else:
                print("Declaration sources differ between TUs for QUSS:", q)
            for tu_path in tu_paths:
                entry = decls_by_tu[tu_path][q]
                print(f"  In TU {tu_path}: {entry}")
            print()
            continue  # skip inconsistent declarations
        # All declaration have same contents and location
        for entry in entries_set:
            header_path = entry[0]
            decls_by_header.setdefault(header_path, {})[q] = entry[1:]

    return decls_by_header


@dataclass
class PrepPassResultStore:
    decls_defined_by_headers: dict[FilePathStr, dict[QUSS, tuple[int, int, FileContentsStr]]]
    decls_defined_after_pp: dict[FilePathStr, dict[QUSS, tuple[int, int, FileContentsStr]]]


def run_preparation_passes(
    original_codebase: Path,
    resultsdir: Path,
    tracker: ingest_tracking.TimingRepo,
    guidance: dict,
    buildcmd: str | None = None,
) -> Path:
    """Returns the path to the final prepared codebase directory."""

    def prep_00_copy_pristine_codebase(pristine: Path, newdir: Path, store: PrepPassResultStore):
        if pristine.is_file():
            newdir.mkdir()
            shutil.copy2(pristine, newdir / pristine.name)
        else:
            copy_codebase(pristine, newdir)

    def prep_01_materialize_compdb(prev: Path, current_codebase: Path, store: PrepPassResultStore):
        with tempfile.TemporaryDirectory() as builddirname:
            builddir = Path(builddirname)
            materialize_compilation_database_in(builddir, current_codebase, buildcmd, tracker)
            # The generated compile_commands.json is in builddir and refers to current_codebase.

            compdb_contents = compdb_path_in(builddir).read_text()
            assert builddirname not in compdb_contents
            assert compdb_contents != "[]", "Generated compile_commands.json is empty"

            shutil.copyfile(compdb_path_in(builddir), compdb_path_in(current_codebase))

        tracker.set_preprocessor_definitions(
            compilation_database.extract_preprocessor_definitions_from_compile_commands(
                compdb_path_in(current_codebase),
                current_codebase,
            ),
        )

        # Miscellaneous unrelated tasks, might as well do them here.
        local_header_paths = set(p.as_posix() for p in current_codebase.glob("**/*.h"))

        store.decls_defined_by_headers = organize_decls_by_headers(
            collect_decls_by_tu(current_codebase, restricted_to_files=local_header_paths)
        )

        print(
            f"PPRC: Collected {sum(len(v) for v in store.decls_defined_by_headers.values())} declarations defined by headers."
        )
        json.dump(
            guidance,
            open(current_codebase / XJ_GUIDANCE_FILENAME, "w", encoding="utf-8"),
            indent=2,
        )

    def prep_localize_mutable_globals(
        prev: Path, current_codebase: Path, store: PrepPassResultStore
    ):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        c_refact.localize_mutable_globals(
            current_codebase / "xj-cclyzer.json", compdb, prev, current_codebase
        )

    def prep_lift_subfield_args(prev: Path, current_codebase: Path, store: PrepPassResultStore):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        c_refact_arglifter.lift_subfield_args(compdb)

    def run_cc2json_or_cached(bitcode_module_path: Path, current_codebase: Path) -> None:
        assert bitcode_module_path.exists()

        cp = hermetic.run(
            ["llvm-nm", str(bitcode_module_path)],
            capture_output=True,
            check=True,
        )

        main_func_defined = cp.stdout.decode().find(" T main") != -1
        # TODO: could also accept guidance to override this decision
        internalize_globals_flag = ["--internalize-globals"] if main_func_defined else []

        json_out_path = current_codebase / "xj-cclyzer.json"

        cache_relevant_cc2json_flags = [
            "--datalog-analysis=unification",
            "--debug-datalog=false",
            "--context-sensitivity=insensitive",
            "--entrypoints=library",  # consider all functions to be reachable
            *internalize_globals_flag,
        ]

        bitcode_hash = hashlib.sha256(bitcode_module_path.read_bytes()).hexdigest()
        cache_signature = [bitcode_hash, WANT["10j-more-deps"], *cache_relevant_cc2json_flags]

        xj_cclyzer_results_cache_path = repo_root.localdir() / "xj-cclyzer-cache.json"
        if xj_cclyzer_results_cache_path.exists():
            cache_data = json.load(open(xj_cclyzer_results_cache_path, "r", encoding="utf-8"))
            print("cached  signature:", cache_data["signature"])
            print("current signature:", cache_signature)
            if cache_data["signature"] == cache_signature:
                print("Reusing cached cclyzer++ analysis results...")
                json.dump(
                    cache_data["contents"], open(json_out_path, "w", encoding="utf-8"), indent=2
                )

                return
            if cache_data["signature"][0] == cache_signature[0]:
                print(
                    "Bitcode matches cached cclyzer++ results, but flags differ; recomputing analysis..."
                )
            else:
                print("Bitcode differs from cached cclyzer++ results; recomputing analysis...")

        print("Running cclyzer++ analysis, this can take a while for larger programs...")
        hermetic.run_command_with_progress(
            [
                "cc2json-llvm14",
                str(bitcode_module_path),
                *cache_relevant_cc2json_flags,
                f"--json-out={json_out_path}",
            ],
            current_codebase / "xj-cc2json-stdout.txt",
            current_codebase / "xj-cc2json-stderr.txt",
            # check=True,
            env_ext={"XJ_USE_LLVM14": "1"},
        )

        contents = json.load(open(json_out_path, "r", encoding="utf-8"))
        json.dump(
            {"signature": cache_signature, "contents": contents},
            open(xj_cclyzer_results_cache_path, "w", encoding="utf-8"),
            indent=2,
        )

    def prep_run_cclzyerpp_analysis(prev: Path, current_codebase: Path, store: PrepPassResultStore):
        # Compile and link LLVM bitcode module
        bitcode_module_path = current_codebase / "linked_module.bc"

        llvm_bitcode_linking.compile_and_link_bitcode(
            compdb_path_in(current_codebase), bitcode_module_path, use_llvm14=True
        )

        run_cc2json_or_cached(bitcode_module_path, current_codebase)

    def prep_uniquify_statics(prev: Path, current_codebase: Path, store: PrepPassResultStore):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        all_pgs_cursors = c_refact.compute_globals_and_statics_for_project(
            compdb, statics_only=True
        )
        all_pgs = [c_refact.mk_NamedDeclInfo(c) for c in all_pgs_cursors]
        # We do not want to try renaming symbols from outside the current codebase!
        current_codebase_dir = current_codebase.as_posix()
        pgs = [v for v in all_pgs if (v.file_path or "").startswith(current_codebase_dir)]
        all_global_names = set(g_s.spelling for g_s in pgs)
        uniquifiers: dict[str, int] = {}

        def mk_unique_name(base: str) -> str:
            while True:
                n = uniquifiers.get(base, 0)
                uniquifiers[base] = n + 1
                candidate = f"{base}_xjtr_{n}"
                if candidate not in all_global_names:
                    return candidate

        rewrites_per_file: dict[str, dict[int, tuple[int, str, str]]] = {}
        pgs_in_determinstic_order = sorted(
            pgs, key=lambda g: (g.file_path or "", g.decl_start_byte_offset)
        )
        for g_s in pgs_in_determinstic_order:
            rewrites_per_file.setdefault(g_s.file_path or "", {})[g_s.decl_start_byte_offset] = (
                g_s.decl_end_byte_offset,
                mk_unique_name(g_s.spelling),
                g_s.spelling,
            )

        for srcfile, editdict in rewrites_per_file.items():
            contents = Path(srcfile).read_bytes()
            edits = []
            for decl_start_byte_offset, (
                decl_end_byte_offset,
                new_name,
                old_name,
            ) in editdict.items():
                name_bytes = old_name.encode("utf-8")

                def find_variant(name_bytes, prefix_bytes: bytes) -> int:
                    idx = contents.find(
                        prefix_bytes + name_bytes, decl_start_byte_offset, decl_end_byte_offset
                    )
                    if idx != -1:
                        return idx + len(prefix_bytes)
                    return -1

                # The variable name might occur in the type name, so we search for it
                # prefixed by something that would count as a token separator.
                separators = [b" ", b"*", b"&", b"\n", b"\t", b"(", b")", b"[", b"]", b","]
                name_byte_offset = -1
                for sep in separators:
                    name_byte_offset = find_variant(name_bytes, sep)
                    if name_byte_offset != -1:
                        break
                if name_byte_offset == -1:
                    # Possibly it's at the start of the range, with a separator after it.
                    if contents.startswith(name_bytes, decl_start_byte_offset):
                        nextbyte = contents[decl_start_byte_offset + len(name_bytes)]
                        if chr(nextbyte).encode() in separators:
                            name_byte_offset = decl_start_byte_offset
                assert name_byte_offset != -1, (
                    f"Could not find bytes for '{old_name}' in source file range: {contents[decl_start_byte_offset:decl_end_byte_offset]!r}"
                )
                edits.extend(["--offset", str(name_byte_offset), "--new-name", new_name])
            hermetic.run(
                [
                    "clang-rename",
                    "-i",  # inplace
                    *edits,
                    srcfile,
                ],
                cwd=current_codebase,
                check=True,
            )

    def prep_pre_refold_consolidation(
        prev: Path, current_codebase: Path, store: PrepPassResultStore
    ):
        # store.decls_defined_by_headers has, for each header, a collection of
        # (quss-ident, (start_offset, end_offset, source_text)) entries.
        # For each translation unit, we'll find the corresonding range for the
        # same quss-ident, and compare it with the header's version.
        # If the header's version is different, we'll record that this TU
        # modified the definition.
        # If every TU modifies the same header-defined declaration in the same way,
        # we will replace each TU's version with the header's version,
        # and replace the header's version with the modified version.
        # (The former allows refolding to work better, the latter preserves
        # the intended semantics of the program.)
        #
        # Since we run this pass before refolding, locations refer to .i files,
        # not .c or .h files.
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )

        from c_refact_type_mod_replicator import quss
        from clang.cindex import CursorKind

        decls_src_by_tu: dict[FilePathStr, dict[QUSS, FileContentsStr]] = {}
        tus_modifying_decls: dict[QUSS, set[FilePathStr]] = {}

        index = c_refact.create_xj_clang_index()
        tus = c_refact.parse_project(index, compdb)

        # Build a map from QUSS to the expected (original) source text from headers
        quss_to_header_src: dict[QUSS, FileContentsStr] = {}
        for header_decls in store.decls_defined_by_headers.values():
            for q, (start_offset, end_offset, source_text) in header_decls.items():
                quss_to_header_src[q] = source_text

        # For each TU, collect the actual source text for each QUSS
        for tu_path, tu in tus.items():
            # quss_to_expanded_src: dict[QUSS, FileContentsStr] = {}
            # print(f"PPRC: Analyzing TU for modified declarations: {tu_path}")
            # print(f"PPRC: decls_defined_after_pp has len ({len(store.decls_defined_after_pp)})")
            # for q, header_decl in store.decls_defined_after_pp.get(tu_path, {}).items():
            #     (start_offset, end_offset, source_text) = header_decl
            #     quss_to_expanded_src[q] = source_text

            tu_file_contents = Path(tu_path).read_text(encoding="utf-8")
            for cursor in tu.cursor.get_children():
                if cursor.kind.is_declaration():
                    if cursor.kind == CursorKind.FUNCTION_DECL and cursor.is_definition():
                        # Skip function definitions
                        continue

                    if (
                        cursor.kind
                        in (
                            CursorKind.STRUCT_DECL,
                            CursorKind.UNION_DECL,
                            CursorKind.ENUM_DECL,
                            CursorKind.TYPEDEF_DECL,
                        )
                        and not cursor.is_definition()
                    ):
                        # Skip forward declarations
                        continue

                    q = quss(cursor, None)
                    if q in quss_to_header_src:
                        start_offset = cursor.extent.start.offset
                        end_offset = cursor.extent.end.offset
                        newest_tu_src = tu_file_contents[start_offset:end_offset]
                        decls_src_by_tu.setdefault(tu_path, {})[q] = newest_tu_src

                        expanded_tu_src = store.decls_defined_after_pp.get(tu_path, {}).get(
                            q, (None, None, None)
                        )[2]
                        # Check if TU's version differs from header's expanded version
                        if q in quss_to_header_src and not expanded_tu_src:
                            print(
                                f"PPRC: WARNING: No expanded header source recorded for QUSS: {q}"
                            )
                        elif newest_tu_src != expanded_tu_src:
                            print(
                                f"PPRC: Found MODIFIED {q} declaration in TU:",
                                tu_path,
                            )
                            print("PPRC: Source text:", newest_tu_src)
                            print("PPRC: extent offsets:", start_offset, end_offset)
                            print("PPRC: extent: ", cursor.extent)

                            tus_modifying_decls.setdefault(q, set()).add(tu_path)

        # Find declarations that are modified identically in all TUs
        for q, modifying_tus in tus_modifying_decls.items():
            # # Check if all TUs modify this declaration
            # if modifying_tus != all_tu_paths:
            #     print("PPRC: Declaration not modified by all TUs for QUSS:", q)
            #     continue  # Not all TUs modify this declaration

            # Check if all TUs modify it in the same way
            modified_versions = set()
            for tu_path in modifying_tus:
                if q in decls_src_by_tu[tu_path]:
                    modified_versions.add(decls_src_by_tu[tu_path][q])

            if len(modified_versions) != 1:
                print("PPRC: Declaration modified differently between TUs for QUSS:", q)
                continue  # TUs modify it in different ways

            # All TUs modify this declaration identically
            # Apply the consolidation: replace TU versions with header version,
            # and update header with the modified version
            modified_version = modified_versions.pop()
            original_header_version = quss_to_header_src[q]
            expanded_header_version = quss_to_expanded_src[q]

            if expanded_header_version != original_header_version:
                print("WARNING: Detected a modified declaration in .i file:", q)
                print(
                    "        for which the header declaration involved detectable preprocessor expansion."
                )
                print("PPRC:   This makes it non-obvious how to map edits back to the header.")
                continue

            print(f"PPRC: Consolidating declaration {q}:")
            print(f"  Original (header):\n{original_header_version}")
            print("----------------------------")
            print(f"  Modified (TUs):\n{modified_version}")
            print("----------------------------")

            # Replace in each TU: modified_version -> original_header_version
            for tu_path in modifying_tus:
                if q in decls_src_by_tu[tu_path]:
                    tu_file_path = Path(tu_path)
                    tu_contents = tu_file_path.read_text(encoding="utf-8")
                    # Replace the modified version with the original header version
                    new_contents = tu_contents.replace(modified_version, original_header_version)
                    if new_contents != tu_contents:
                        tu_file_path.write_text(new_contents, encoding="utf-8")

            # Update the header with the modified version
            for header_path, header_decls in store.decls_defined_by_headers.items():
                if q in header_decls:
                    start_offset, end_offset, _ = header_decls[q]
                    # TODO handle nested header locations
                    header_file_path = current_codebase / Path(header_path).name
                    if header_file_path.exists():
                        header_contents = header_file_path.read_text(encoding="utf-8")
                        # Replace original with modified version
                        new_header_contents = (
                            header_contents[:start_offset]
                            + modified_version
                            + header_contents[end_offset:]
                        )
                        header_file_path.write_text(new_header_contents, encoding="utf-8")

    def prep_expand_preprocessor(prev: Path, current_codebase: Path, store: PrepPassResultStore):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        new_compdb: compilation_database.CompileCommands = (
            c_refact.preprocess_and_create_new_compdb(
                compdb,
                current_codebase.as_posix(),
            )
        )
        # new_compdb already written to current_codebase
        for cmd in new_compdb.commands:
            assert cmd.file_path.suffix == ".i", "Expected preprocessed .i files"

        store.decls_defined_after_pp = collect_decls_by_tu(current_codebase)

        print(
            f"PPRC: Collected {sum(len(v) for v in store.decls_defined_after_pp.values())} declarations defined after preprocessing."
        )
        print("!!!!!!!!!!!!!!!!!!!!!!")
        print(json.dumps(store.decls_defined_after_pp, indent=2))
        print("!!!!!!!!!!!!!!!!!!!!!!")

    def prep_refold_preprocessor(prev: Path, current_codebase: Path, store: PrepPassResultStore):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        new_compdb: compilation_database.CompileCommands = (
            c_refact.refold_preprocess_and_create_new_compdb(compdb, current_codebase.as_posix())
        )
        # new_compdb already written to current_codebase
        for cmd in new_compdb.commands:
            assert cmd.file_path.suffix == ".c", "Expected un-preprocessed .c files"

    preparation_passes: list[
        tuple[str, Callable[[Path, Path, PrepPassResultStore], CompletedProcess | None]]
    ] = [
        ("copy_pristine_codebase", prep_00_copy_pristine_codebase),
        ("materialize_compdb", prep_01_materialize_compdb),
        ("uniquify_statics", prep_uniquify_statics),
        ("expand_preprocessor", prep_expand_preprocessor),
        ("run_cclzyerpp_analysis", prep_run_cclzyerpp_analysis),
        ("localize_mutable_globals", prep_localize_mutable_globals),
        ("lift_subfield_args", prep_lift_subfield_args),
        ("pre_refold_consolidation", prep_pre_refold_consolidation),
        # ("refold_preprocessor", prep_refold_preprocessor),
    ]

    prev = original_codebase.absolute()
    resultsdir_abs = resultsdir.absolute()

    store = PrepPassResultStore(decls_defined_by_headers={}, decls_defined_after_pp={})

    # Note: the original codebase may be a file or directory,
    # but after the first round, `prev` always refers to a directory.
    for counter, (tag, func) in enumerate(preparation_passes, start=0):
        newdir = resultsdir_abs / f"c_{counter:02d}_{tag}"
        with tracker.tracking(f"preparation_pass_{counter:02d}_{tag}", newdir) as step:
            start_ns = time.perf_counter_ns()
            if counter > 0:
                copy_codebase(prev, newdir)
            cp_or_None: CompletedProcess | None = func(prev, newdir, store)
            if cp_or_None is not None:
                step.update_sub(cp_or_None)
            end_ns = time.perf_counter_ns()

            elapsed_ms = round(elapsed_ms_of_ns(start_ns, end_ns))
            print(f"Preparation pass {counter} ({tag}) took {elapsed_ms} ms")

            prev = newdir

    return prev
