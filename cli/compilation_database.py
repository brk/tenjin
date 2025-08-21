import json
from pathlib import Path
import shlex

from repo_root import localdir
import hermetic
import ingest


def write_synthetic_compile_commands_to(compdb_path: Path, c_file: Path, builddir: Path):
    """Write a synthetic compile_commands.json file for a single C file."""
    assert compdb_path.parent.is_dir()
    outname = c_file.with_suffix(".o").name
    cc = hermetic.xj_llvm_root(localdir()) / "bin" / "clang"
    c_file_full_q = shlex.quote(c_file.resolve().as_posix())
    contents = json.dumps(
        [
            {
                "directory": builddir.as_posix(),
                "command": f"{cc} -c {c_file_full_q} -o {shlex.quote(outname)}",
                "file": c_file.resolve().as_posix(),
                "output": outname,
            }
        ],
        indent=2,
    )
    compdb_path.write_text(contents, encoding="utf-8")


def extract_preprocessor_definitions_from_compile_commands(
    parsed_compile_commands: list[dict],
    codebase: Path,
) -> ingest.PerFilePreprocessorDefinitions:
    """Extract preprocessor definitions from `compile_commands.json`"""
    definitions = {}
    for command_info in parsed_compile_commands:
        command_str = command_info.get("command", "")
        # command_info["directory"] is build directory, which can be
        # located anywhere; it has no relation to the source file path.
        if Path(command_info.get("file", "")).resolve() == codebase.resolve():
            relative_path = command_info.get("file", "")
        else:
            relative_path = Path(command_info.get("file", "")).relative_to(codebase)
        defs: list[ingest.PreprocessorDefinition] = []
        args = shlex.split(command_str)
        i = 0
        while i < len(args):
            arg = args[i]
            i += 1
            if arg == "-D" and i + 1 < len(args):
                # If we find a -D, the next argument is a definition.
                key, _, value = args[i + 1].partition("=")
                i += 1  # Skip the value
                defs.append((key, value))
            elif arg.startswith("-D") and "=" in arg:
                # Handle -Dkey=value style definitions.
                key, _, value = arg[2:].partition("=")
                defs.append((key, value))
            if arg.startswith("-D"):
                # Handle -Dkey style definitions.
                defs.append((arg[2:], None))  # Add the definition without the -D prefix
        if defs:
            definitions[relative_path.as_posix()] = defs
    return definitions
