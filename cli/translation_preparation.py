import shutil
import time
import tempfile
from pathlib import Path
from typing import Callable
from subprocess import CompletedProcess

import click

import compilation_database
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


def run_copy_existing_compdb(_codebase: Path, _builddir: Path) -> CompletedProcess | None:
    """Copy an existing compile_commands.json from the codebase."""
    provided_compdb = _codebase / "compile_commands.json"
    if provided_compdb.exists():
        shutil.copyfile(provided_compdb, _builddir / "compile_commands.json")
    return None


def run_preparation_passes(
    original_codebase: Path,
    resultsdir: Path,
    tracker: ingest_tracking.TimingRepo,
    buildcmd: str | None = None,
) -> Path:
    """Returns the path to the final prepared codebase directory."""

    def compdb_path_in(dir: Path) -> Path:
        return dir / "compile_commands.json"

    def prep_00_copy_pristine_codebase(pristine: Path, newdir: Path):
        if pristine.is_file():
            newdir.mkdir()
            shutil.copy2(pristine, newdir / pristine.name)
        else:
            shutil.copytree(pristine, newdir)

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

    preparation_passes: list[tuple[str, Callable[[Path, Path], CompletedProcess | None]]] = [
        ("copy_pristine_codebase", prep_00_copy_pristine_codebase),
        ("materialize_compdb", prep_01_materialize_compdb),
        ("run_cclzyerpp_analysis", prep_run_cclzyerpp_analysis),
    ]

    prev = original_codebase

    # Note: the original codebase may be a file or directory,
    # but after the first round, `prev` always refers to a directory.
    for counter, (tag, func) in enumerate(preparation_passes, start=0):
        newdir = resultsdir / f"c_{counter:02d}_{tag}"
        with tracker.tracking(f"preparation_pass_{counter:02d}_{tag}", newdir) as step:
            start_ns = time.perf_counter_ns()
            if counter > 0:
                shutil.copytree(prev, newdir)
                if counter > 1:
                    compilation_database.rebase_compile_commands_from_to(
                        compdb_path_in(newdir), prev, newdir
                    )
            cp_or_None: CompletedProcess | None = func(prev, newdir)
            if cp_or_None is not None:
                step.update_sub(cp_or_None)
            end_ns = time.perf_counter_ns()

            elapsed_ms = round(elapsed_ms_of_ns(start_ns, end_ns))
            print(f"Preparation pass {counter} ({tag}) took {elapsed_ms} ms")

            prev = newdir

    return prev
