import shutil
import tempfile
from pathlib import Path
import re

import hermetic


def do_translate(
    root: Path,
    codebase: Path,
    resultsdir: Path,
    cratename: str,
    c_main_in: str | None = None,
    guidance: str | None = None,
):
    """
    Translate a codebase from C to Rust.

    The input directory should have a pre-generated compile_commands.json file,
    or an easy means of generating one (such as a CMakeLists.txt file).

    The `resultsdir` directory will contain subdirectories with intermediate
    stages of the resulting translation. Assuming no errors occurred, the final
    translation will be in the `final` subdirectory. The `resultsdir` will also
    (eventually)
    have a file called `ingest.json` containing metadata about the translation.
    """

    def perform_pre_translation(builddir: Path) -> Path:
        provided_compdb = codebase / "compile_commands.json"
        provided_cmakelists = codebase / "CMakeLists.txt"

        if provided_cmakelists.exists() and not provided_compdb.exists():
            # If we have a CMakeLists.txt, we can generate the compile_commands.json
            hermetic.run(
                [
                    "cmake",
                    "-S",
                    str(codebase),
                    "-B",
                    str(builddir),
                    "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
                ],
                check=True,
            )
            return builddir / "compile_commands.json"
        else:
            # Otherwise, we assume the compile_commands.json is already present
            return provided_compdb

    c2rust_transpile_flags = [
        "--translate-const-macros",
        "--reduce-type-annotations",
        "--disable-refactoring",
        "--log-level",
        "INFO",
        "--guidance",
        guidance if guidance else "{}",
    ]

    if c_main_in:
        c2rust_transpile_flags.extend(["--binary", c_main_in.removesuffix(".c")])

    with tempfile.TemporaryDirectory() as builddirname:
        builddir = Path(builddirname)
        compdb = perform_pre_translation(builddir)

        c2rust_bin = root / "c2rust" / "target" / "debug" / "c2rust"

        # The crate name that c2rust uses is based on the directory stem,
        # so we create a subdirectory with the desired crate name.
        output = resultsdir / cratename
        output.mkdir(parents=True, exist_ok=False)
        hermetic.run(
            [str(c2rust_bin), "transpile", str(compdb), "-o", str(output), *c2rust_transpile_flags],
            check=True,
            env_ext={
                "RUST_BACKTRACE": "1",
            },
        )
        # Normalize the unmodified translation results to end up
        # in a directory with a project-independent name.
        renamed_output = resultsdir / "out_00"
        output.rename(renamed_output)

        # Eventually, we'll add improvement passes here which will
        # generate additional intermediate `out_XX` directories.

        # Find the highest numbered output directory and copy its contents
        # to the final output directory.
        highest_out = find_highest_output_dir(resultsdir)
        if highest_out is not None:
            shutil.copytree(
                highest_out,
                resultsdir / "final",
            )


def find_highest_output_dir(base: Path) -> Path | None:
    """
    Find the directory matching pattern 'out_X' with the largest X.
    """
    pattern = re.compile(r"^out_(\d+)$")
    base = Path(base)

    if not base.exists():
        return None

    max_num = -1
    latest_dir = None

    for item in base.iterdir():
        if item.is_dir():
            match = pattern.match(item.name)
            if match:
                num = int(match.group(1))
                if num > max_num:
                    max_num = num
                    latest_dir = item

    return latest_dir if latest_dir else None
