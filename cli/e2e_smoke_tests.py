# Simple end-to-end smoke tests for Tenjin translation,
# as Python code instead of shell scripts.

import tempfile
import time
from pathlib import Path

import hermetic
import repo_root
import translation
import cli_subcommands


def query_selected_tests(testnames: list[str]):
    """Return a mapping of available smoke test names to callables.

    Values may be a callable (to run the test) or None if not implemented.
    """
    available_tests = {
        "1": e2e_smoke_test_1,
        "2": e2e_smoke_test_2,
        "3": e2e_smoke_test_3,
    }

    if not testnames or testnames == ["all"]:
        testnames = sorted(list(available_tests.keys()))

    return {name: available_tests.get(name) for name in testnames}


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

        def run_cargo_on_final(args: list[str], capture_output: bool = False):
            return hermetic.run_cargo_on_translated_code(
                args,
                cwd=results_dir / "final",
                check=True,
                capture_output=capture_output,
            )

        run_cargo_on_final(["build"])
        rs_prog_output = run_cargo_on_final(["run"], capture_output=True)

        assert rs_prog_output.stdout == c_prog_output.stdout
        if hermetic.running_in_ci():
            print("::endgroup::")

        print("TENJIN_SMOKE_TEST_SUMMARY: e2e_smoke_test_1 output matches for C and Rust")
        print(f"TENJIN_SMOKE_TEST_SUMMARY: e2e_smoke_test_1 took {elapsed_s} seconds to translate")


def e2e_smoke_test_2():
    """Validation that CodeHawk-C analysis runs successfully"""
    if hermetic.running_in_ci():
        print("::group::")

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir_path = Path(tempdir)

        (tempdir_path / "main.c").write_text(
            """
            /* #include <stdio.h> */
            /* CodeHawk-C uses CIL which cannot parse Mac system headers.
               For now we avoid the issue by using a forward declaration
               instead of a real #include */
            extern void printf(const char* format, ...);

            int main(int argc, char** argv) {
                for (int i = 0; i < argc; i++) {
                    printf("Arg %d: %s\\n", i, argv[i]);
                }
                return 0;
            }
            """
        )

        hermetic.run_chkc(["c-file", "parse", str(tempdir_path / "main.c")], check=True)
        hermetic.run_chkc(["c-file", "analyze", str(tempdir_path / "main.c")], check=True)
        hermetic.run_chkc(["c-file", "report", str(tempdir_path / "main.c")], check=True)

    if hermetic.running_in_ci():
        print("::endgroup::")


def e2e_smoke_test_3():
    """Test translation of a multi-file C project with static variables and helpers."""
    if hermetic.running_in_ci():
        print("::group::")
    root = repo_root.find_repo_root_dir_Path()
    cli_subcommands.do_build_rs(root)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir_path = Path(tempdir)
        codebase_dir = tempdir_path / "codebase"
        results_dir = tempdir_path / "results"

        # Create the codebase
        codebase_dir.mkdir()

        (codebase_dir / "main.c").write_text(
            """int a_1();
int a_2();
int b_1();
int a_help();
int b_help();

#include <stdio.h>

int main() {
  printf("a_1 => %d\\n", a_1());
  printf("a_1 => %d\\n", a_1());
  printf("a_1 => %d\\n", a_1());
  printf("a_2 => %d\\n", a_2());
  printf("b_1 => %d\\n", b_1());
  printf("b_1 => %d\\n", b_1());
  printf("a_2 => %d\\n", a_2());
  printf("a_help => %d\\n", a_help());
  printf("b_help => %d\\n", b_help());
  return 0;
}
"""
        )

        (codebase_dir / "a.c").write_text(
            """int a_1() {
  static int foo = 0;
  return foo++;
}

int a_2() {
  static int foo = 0;
  return foo++;
}

static int helper() { return 42; }

int a_help() {
\treturn helper();
}
"""
        )

        (codebase_dir / "b.c").write_text(
            """int b_1() {
  static int foo = 0;
  return foo++;
}

static int helper() { return 100; }
int b_help() { return helper(); }
"""
        )

        (codebase_dir / "Makefile").write_text(
            """all:
\tclang -c main.c -o main.o
\tclang -c a.c -o a.o
\tclang -c b.c -o b.o
\tclang a.o b.o main.o -o made_main

"""
        )

        # For reasons I don't understand yet, intercept-build sometimes creates an empty
        # compile_commands.json on macOS.
        hermetic.run(
            ["clang", "-c", "a.c", "b.c", "main.c", "-MJ", "compile_commands.json"],
            cwd=codebase_dir,
            check=True,
        )
        # -MJ produces a comma-terminated list; fix it to be valid JSON
        compdb_path = codebase_dir / "compile_commands.json"
        compdb_path.write_text("[\n" + compdb_path.read_text().rstrip(",\n") + "\n]\n")

        hermetic.run(["make"], cwd=codebase_dir, check=True)

        assert (codebase_dir / "compile_commands.json").exists()

        # Build and run the C program
        c_prog_output = hermetic.run(
            [str(codebase_dir / "made_main")],
            check=True,
            capture_output=True,
        )

        start_time = time.time()
        # Run translation
        translation.do_translate(
            root,
            codebase_dir,
            results_dir,
            cratename="smoke_test_3",
            guidance_path_or_literal="{}",
            c_main_in="main.c",
        )

        elapsed_s = int((time.time() - start_time))

        assert (results_dir / "final" / "Cargo.toml").exists()

        def run_cargo_on_final(args: list[str], capture_output: bool = False):
            return hermetic.run_cargo_on_translated_code(
                args,
                cwd=results_dir / "final",
                check=True,
                capture_output=capture_output,
            )

        run_cargo_on_final(["build"])
        rs_prog_output = run_cargo_on_final(["run"], capture_output=True)

        assert rs_prog_output.stdout == c_prog_output.stdout
        if hermetic.running_in_ci():
            print("::endgroup::")

        print("TENJIN_SMOKE_TEST_SUMMARY: e2e_smoke_test_3 output matches for C and Rust")
        print(f"TENJIN_SMOKE_TEST_SUMMARY: e2e_smoke_test_3 took {elapsed_s} seconds to translate")
