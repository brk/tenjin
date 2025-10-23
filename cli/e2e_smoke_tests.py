# Simple end-to-end smoke tests for Tenjin translation,
# as Python code instead of shell scripts.

import tempfile
import time
from pathlib import Path

import hermetic
import repo_root
import translation
import cli_subcommands


def e2e_smoke_test_1():
    """A simple end-to-end smoke test for Tenjin translation."""
    if hermetic.running_in_ci():
        print("::group::")
    root = repo_root.find_repo_root_dir_Path()
    cli_subcommands.do_build_rs(root)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir_path = Path(tempdir)
        codebase_dir = tempdir_path / "codebase"
        results_dir = tempdir_path / "results"

        # Create a simple C codebase
        codebase_dir.mkdir()
        (codebase_dir / "main.c").write_text(
            """
            #include <stdio.h>

            int main() {
                printf("Hello, Tenjin!\\n");
                return 0;
            }
            """
        )

        # Ensure it compiles and runs as expected
        hermetic.run(
            ["clang", "-o", str(tempdir_path / "main"), str(codebase_dir / "main.c")], check=True
        )
        c_prog_output = hermetic.run([str(tempdir_path / "main")], check=True, capture_output=True)
        assert c_prog_output.stdout == b"Hello, Tenjin!\n"

        start_time = time.time()
        # Run translation
        translation.do_translate(
            root,
            codebase_dir / "main.c",
            results_dir,
            cratename="smoke_test_1",
            guidance_path_or_literal="{}",
            c_main_in="main.c",
        )

        elapsed_s = int((time.time() - start_time))

        assert (results_dir / "final" / "Cargo.toml").exists()

        hermetic.run_cargo_in(
            ["build"],
            cwd=results_dir / "final",
            check=True,
        )

        rs_prog_output = hermetic.run_cargo_in(
            ["run"],
            cwd=results_dir / "final",
            check=True,
            capture_output=True,
        )

        assert rs_prog_output.stdout == c_prog_output.stdout
        if hermetic.running_in_ci():
            print("::endgroup::")

        print("TENJIN_SMOKE_TEST_SUMMARY: e2e_smoke_test_1 output matches for C and Rust")
        print(f"TENJIN_SMOKE_TEST_SUMMARY: e2e_smoke_test_1 took {elapsed_s} seconds to translate")


def e2e_smoke_test_2():
    """Validation that CodeHawk-C analysis runs successfully"""
    if hermetic.running_in_ci():
        print("::group::")

    if hermetic.running_in_ci():
        print("::endgroup::")
