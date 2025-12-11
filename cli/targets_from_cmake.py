from typing import Any, cast
from dataclasses import dataclass
from pathlib import Path
import os.path

import bencodepy  # type: ignore
from cmake_file_api import CMakeProject, ObjectKind
from cmake_file_api.kinds.codemodel.v2 import CodemodelV2
from cmake_file_api.kinds.codemodel.target.v2 import (
    CodemodelTargetV2,
    TargetType,
    LinkFragmentRole,
)


# Path.resolve() resolves symlinks, but we want to preserve them
def combine(path1: str, path2: str) -> str:
    if os.path.isabs(path2):
        return os.path.normpath(path2)
    return os.path.normpath(os.path.join(path1, path2))


@dataclass
class EntryInfo:
    entry: dict[str, Any]
    new_args: list[str]
    c_inputs: list[str]
    rest_inputs: list[str]
    libs: list[str]
    lib_dirs: list[str]
    compile_only: bool
    shared_lib: bool
    output: str | None = None


def convert_json_entries(entries: list[dict[str, Any]]) -> list[EntryInfo]:
    entry_infos = []
    for entry in entries:
        old_args, entry["arguments"] = entry["arguments"], []
        arg_iter = iter(old_args)

        ei = EntryInfo(
            entry=entry,
            new_args=[],
            c_inputs=[],
            rest_inputs=[],
            libs=[],
            lib_dirs=[],
            compile_only=False,
            shared_lib=False,
            output=None,
        )

        ei.new_args.append(next(arg_iter))  # Copy over old_args[0]
        for arg in arg_iter:
            if arg in {"-D", "-U", "-I", "-include"}:
                # TODO: use the full list of `Separate` options from gcc
                ei.new_args.append(arg)
                ei.new_args.append(next(arg_iter))

            elif arg == "-c":
                ei.compile_only = True

            elif arg == "-o":
                ei.output = next(arg_iter)
                ei.new_args.append(arg)
                assert ei.output is not None
                ei.new_args.append(ei.output)

            elif arg[:2] == "-o":
                ei.output = arg[2:]
                ei.new_args.append(arg)

            elif arg == "-l":
                ei.libs.append(next(arg_iter))

            elif arg[:2] == "-l":
                ei.libs.append(arg[2:])

            # -pthread implicitly adds -lpthread
            elif arg == "-pthread":
                ei.libs.append("pthread")
                ei.new_args.append(arg)

            elif arg == "-L":
                ei.lib_dirs.append(next(arg_iter))

            elif arg[:2] == "-L":
                ei.lib_dirs.append(arg[2:])

            elif arg == "-shared":
                ei.shared_lib = True

            elif arg[-2:] == ".c":
                ei.c_inputs.append(arg)

            elif arg[-2:] == ".o":
                ei.rest_inputs.append(arg)

            else:
                ei.new_args.append(arg)

        entry_infos.append(ei)
    return entry_infos


def cmake_project_to_entry_infos(cmake_project: CMakeProject) -> list[EntryInfo]:
    """
    Extract EntryInfo values for shared library and executable targets
    from a CMakeProject.
    """
    results = cmake_project.cmake_file_api.inspect_all()
    codemodel_v2 = cast(CodemodelV2, results[ObjectKind.CODEMODEL][2])

    entry_infos: list[EntryInfo] = []

    for config in codemodel_v2.configurations:
        for cmake_target in config.targets:
            target: CodemodelTargetV2 = cmake_target.target

            # Only process SHARED and EXECUTABLE targets
            if target.type not in (TargetType.SHARED, TargetType.EXECUTABLE):
                continue

            entry: dict[str, Any] = {
                "directory": str(target.paths.source),
                "type": "cc",
            }
            ei = EntryInfo(
                entry=entry,
                new_args=[],
                c_inputs=[],
                rest_inputs=[],
                libs=[],
                lib_dirs=[],
                compile_only=False,
                shared_lib=False,
                output=None,
            )

            # Set output
            ei.output = target.nameOnDisk if target.nameOnDisk else target.name
            ei.shared_lib = target.type == TargetType.SHARED

            # Collect source files
            for source in target.sources:
                source_path = str(source.path)
                if (
                    source_path.endswith(".c")
                    or source_path.endswith(".cpp")
                    or source_path.endswith(".cc")
                ):
                    ei.c_inputs.append(source_path)
                elif not source_path.endswith(".h") and not source_path.endswith(".hpp"):
                    ei.rest_inputs.append(source_path)

            # Extract compile arguments from compile groups
            for compile_group in target.compileGroups:
                # Add include paths
                for inc in compile_group.includes:
                    ei.new_args.append("-I")
                    ei.new_args.append(str(inc.path))

                # Add defines
                for define in compile_group.defines:
                    ei.new_args.append(f"-D{define.define}")

                # Add compile command fragments
                for compfrag in compile_group.compileCommandFragments:
                    ei.new_args.append(compfrag.fragment)

            # Extract link info
            if target.link:
                for linkfrag in target.link.commandFragments:
                    if linkfrag.role == LinkFragmentRole.LINK_LIBRARIES:
                        lib = linkfrag.fragment.strip()
                        if lib.startswith("-l"):
                            ei.libs.append(lib[2:])
                        elif lib:
                            ei.rest_inputs.append(lib)
                    elif linkfrag.role == LinkFragmentRole.LIBRARY_PATHS:
                        lib_dir = linkfrag.fragment.strip()
                        if lib_dir.startswith("-L"):
                            ei.lib_dirs.append(lib_dir[2:])
                        elif lib_dir:
                            ei.lib_dirs.append(lib_dir)
                    elif linkfrag.role == LinkFragmentRole.LINK_FLAGS:
                        frag = linkfrag.fragment.strip()
                        if frag:
                            ei.new_args.append(frag)

            entry_infos.append(ei)

    return entry_infos


# Extracted from c2rust/scripts/convert_build_commands.py
def extract_link_compile_commands(
    entry_infos: list[EntryInfo], codebase: Path, builddir: Path
) -> list[dict]:
    fake_ctr = -1

    def get_fake() -> int:
        nonlocal fake_ctr
        fake_ctr += 1
        return fake_ctr

    object_map = {}
    new_cc_entries = []
    new_link_entries = []
    for ei in entry_infos:
        for inp in ei.c_inputs:
            inp_path = Path(inp)
            if not inp_path.is_absolute():
                abs = codebase / inp_path
                if abs.exists():
                    inp_path = abs
                else:
                    d = Path(ei.entry["directory"])
                    abs = codebase / d / inp_path
                    if abs.exists():
                        inp_path = abs

            assert inp_path.exists(), f"Input file {inp_path} does not exist"
            assert inp_path.is_absolute()
            assert inp_path.is_relative_to(codebase)

            # TODO: handle duplicates
            c_object_unresolved = ei.output or "%s___%d.o" % (Path(inp).stem, get_fake())
            c_object = resolve_in_ei_directory(ei, c_object_unresolved, builddir)
            object_map[inp] = c_object

            new_entry = ei.entry.copy()
            new_entry["arguments"] = [*ei.new_args, "-c", inp]
            new_entry["file"] = inp_path.as_posix()  # .relative_to(codebase).as_posix()
            new_entry["output"] = c_object
            del new_entry["type"]
            new_cc_entries.append(new_entry)

    for ei in filter(lambda ei: not ei.compile_only, entry_infos):
        new_entry = ei.entry.copy()
        # mapped_c_objects = list(set([object_map[inp] for inp in ei.c_inputs]))
        # c_files = [inp for inp in ei.c_inputs if inp in object_map]
        # print("c_inputs:", ei.c_inputs)
        # print("mapped_c_objects:", mapped_c_objects)
        # print("c_files:", c_files)
        # print()
        assert not ei.c_inputs

        new_entry["arguments"] = ei.new_args
        # Hacky solution: c2rust-tranpile needs an absolute path here,
        # so we add a path-like prefix so that the transpiler can both
        # parse it correctly and recognize it as a bencoded link command
        inputs = [resolve_in_ei_directory(ei, x, builddir) for x in ei.rest_inputs]
        link_info = {
            "inputs": inputs,  # FIXME: wrong order???
            "c_files": [],
            "libs": ei.libs,
            "lib_dirs": ei.lib_dirs,
            "type": "shared" if ei.shared_lib else "exe",
            # TODO: parse and add in other linker flags
            # for now, we don't do this because rustc doesn't use them
        }
        print("Link info:", link_info)
        # new_entry["_c2rust_link"] = link_info  # delay bencoding, to allow editing of paths
        new_entry["file"] = "/c2rust/link/" + bencodepy.encode(link_info).decode("utf-8")
        # new_entry["file"] = "/c2rust/link"
        output = ei.output or "a.out"
        # CMake's compile_commands.json output paths are relative to the build directory,
        # even when the "directory" is a subdirectory of the build directory.
        new_entry["output"] = resolve_in_ei_directory(ei, output, builddir)
        del new_entry["type"]
        new_link_entries.append(new_entry)

    # With one link command, we won't want to create a workspace.
    if len(new_link_entries) == 1:
        return new_cc_entries

    return sorted(new_cc_entries + new_link_entries, key=lambda e: e["file"])


def resolve_in_ei_directory(ei: EntryInfo, pathbuf: str, builddir: Path) -> str:
    if Path(pathbuf).is_absolute():
        return pathbuf

    p = Path(ei.entry["directory"], pathbuf)
    if not p.exists():
        return pathbuf
    p = p.resolve()
    builddir = builddir.resolve()
    assert p.is_relative_to(builddir)
    return p.relative_to(builddir).as_posix()


def collect_executable_target_names(
    cmake_project: CMakeProject,
    mut_cmake_exe_targets: list[str] | None,
) -> None:
    """
    Collect names of executable targets from a CMakeProject
    into the provided list.
    """
    if mut_cmake_exe_targets is None:
        return

    results = cmake_project.cmake_file_api.inspect_all()
    codemodel_v2 = cast(CodemodelV2, results[ObjectKind.CODEMODEL][2])

    for config in codemodel_v2.configurations:
        for cmake_target in config.targets:
            target: CodemodelTargetV2 = cmake_target.target

            # Only process EXECUTABLE targets
            if target.type != TargetType.EXECUTABLE:
                continue

            mut_cmake_exe_targets.append(target.name)
