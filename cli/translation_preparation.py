import shutil
import time
import tempfile
from pathlib import Path
from typing import Callable
from subprocess import CompletedProcess

import click

import compilation_database
import c_refact
import hermetic
import ingest_tracking
import llvm_bitcode_linking


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


def run_preparation_passes(
    original_codebase: Path,
    resultsdir: Path,
    tracker: ingest_tracking.TimingRepo,
    buildcmd: str | None = None,
) -> Path:
    """Returns the path to the final prepared codebase directory."""

    def prep_00_copy_pristine_codebase(pristine: Path, newdir: Path):
        if pristine.is_file():
            newdir.mkdir()
            shutil.copy2(pristine, newdir / pristine.name)
        else:
            copy_codebase(pristine, newdir)

    def prep_01_materialize_compdb(prev: Path, current_codebase: Path):
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

    def prep_localize_mutable_globals(prev: Path, current_codebase: Path):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        c_refact.localize_mutable_globals(
            current_codebase / "xj-cclyzer.json", compdb, prev, current_codebase
        )

    def prep_run_cclzyerpp_analysis(prev: Path, current_codebase: Path):
        # Compile and link LLVM bitcode module
        bitcode_module_path = current_codebase / "linked_module.bc"

        llvm_bitcode_linking.compile_and_link_bitcode(
            compdb_path_in(current_codebase), bitcode_module_path
        )

        assert bitcode_module_path.exists()

        json_out_path = current_codebase / "xj-cclyzer.json"
        hermetic.run(
            [
                "cc2json",
                str(bitcode_module_path),
                "--datalog-analysis=unification",
                "--debug-datalog=false",
                "--context-sensitivity=insensitive",
                f"--json-out={json_out_path}",
            ],
            check=True,
        )
        click.echo(json_out_path.read_text())

    def prep_uniquify_statics(prev: Path, current_codebase: Path):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        all_pgs = c_refact.compute_globals_and_statics_for_project(compdb)
        # We do not want to try renaming symbols from outside the current codebase!
        current_codebase_dir = current_codebase.as_posix()
        pgs = {k: v for k, v in all_pgs.items() if v.file_path.startswith(current_codebase_dir)}
        all_global_names = set(g_s.spelling for g_s in pgs.values())
        uniquifiers: dict[str, int] = {}

        def mk_unique_name(base: str) -> str:
            while True:
                n = uniquifiers.get(base, 0)
                uniquifiers[base] = n + 1
                candidate = f"{base}_xjtr_{n}"
                if candidate not in all_global_names:
                    return candidate

        rewrites_per_file: dict[str, dict[int, tuple[int, str, str]]] = {}
        for _fqsymname, g_s in pgs.items():
            rewrites_per_file.setdefault(g_s.file_path, {})[g_s.decl_start_byte_offset] = (
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
                separators = [b" ", b"*", b"&", b"\n", b"\t", b"(", b")", b","]
                name_byte_offset = -1
                for sep in separators:
                    name_byte_offset = find_variant(name_bytes, sep)
                    if name_byte_offset != -1:
                        break
                assert name_byte_offset != -1, (
                    f"Could not find bytes for '{old_name}' in source file range"
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

        hermetic.run("cat *.c", cwd=current_codebase, shell=True, check=True)

    def prep_expand_preprocessor(prev: Path, current_codebase: Path):
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

    def prep_refold_preprocessor(prev: Path, current_codebase: Path):
        compdb = compilation_database.CompileCommands.from_json_file(
            compdb_path_in(current_codebase)
        )
        new_compdb: compilation_database.CompileCommands = (
            c_refact.preprocess_and_create_new_compdb(compdb, current_codebase.as_posix())
        )
        # new_compdb already written to current_codebase
        for cmd in new_compdb.commands:
            assert cmd.file_path.suffix == ".c", "Expected un-preprocessed .c files"

    preparation_passes: list[tuple[str, Callable[[Path, Path], CompletedProcess | None]]] = [
        ("copy_pristine_codebase", prep_00_copy_pristine_codebase),
        ("materialize_compdb", prep_01_materialize_compdb),
        ("uniquify_statics", prep_uniquify_statics),
        # ("refold_preprocessor", prep_refold_preprocessor),
        ("expand_preprocessor", prep_expand_preprocessor),
        ("run_cclzyerpp_analysis", prep_run_cclzyerpp_analysis),
        ("localize_mutable_globals", prep_localize_mutable_globals),
    ]

    prev = original_codebase.absolute()
    resultsdir_abs = resultsdir.absolute()

    # Note: the original codebase may be a file or directory,
    # but after the first round, `prev` always refers to a directory.
    for counter, (tag, func) in enumerate(preparation_passes, start=0):
        newdir = resultsdir_abs / f"c_{counter:02d}_{tag}"
        with tracker.tracking(f"preparation_pass_{counter:02d}_{tag}", newdir) as step:
            start_ns = time.perf_counter_ns()
            if counter > 0:
                copy_codebase(prev, newdir)
            cp_or_None: CompletedProcess | None = func(prev, newdir)
            if cp_or_None is not None:
                step.update_sub(cp_or_None)
            end_ns = time.perf_counter_ns()

            elapsed_ms = round(elapsed_ms_of_ns(start_ns, end_ns))
            print(f"Preparation pass {counter} ({tag}) took {elapsed_ms} ms")

            prev = newdir

    return prev
