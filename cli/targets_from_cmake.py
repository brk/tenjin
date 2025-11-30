from typing import Any, Optional, cast
from dataclasses import dataclass
import bencodepy

from cmake_file_api import CMakeProject, ObjectKind
from cmake_file_api.kinds.codemodel.v2 import CodemodelV2
from cmake_file_api.kinds.codemodel.target.v2 import (
    CodemodelTargetV2,
    TargetType,
    LinkFragmentRole,
)


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
    output: Optional[str] = None


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
                for frag in compile_group.compileCommandFragments:
                    ei.new_args.append(frag.fragment)

            # Extract link info
            if target.link:
                for frag in target.link.commandFragments:
                    if frag.role == LinkFragmentRole.LINK_LIBRARIES:
                        lib = frag.fragment.strip()
                        if lib.startswith("-l"):
                            ei.libs.append(lib[2:])
                        elif lib:
                            ei.rest_inputs.append(lib)
                    elif frag.role == LinkFragmentRole.LIBRARY_PATHS:
                        lib_dir = frag.fragment.strip()
                        if lib_dir.startswith("-L"):
                            ei.lib_dirs.append(lib_dir[2:])
                        elif lib_dir:
                            ei.lib_dirs.append(lib_dir)
                    elif frag.role == LinkFragmentRole.LINK_FLAGS:
                        ei.new_args.append(frag.fragment.strip())

            entry_infos.append(ei)

    return entry_infos


# Extracted from c2rust/scripts/convert_build_commands.py
def extract_link_compile_commands(
    entry_infos: list[EntryInfo], out_dir: str | None = None
) -> list[dict]:
    object_map = {}
    new_entries = []
    # for ei in entry_infos:
    #     for inp in ei.c_inputs:
    #         # inp_path = os.path.join(entry["directory"], inp)
    #         # inp_path = os.path.realpath(inp_path)

    #         # TODO: handle duplicates
    #         # c_object = ei.output or "%s_%d.o" % (inp[:-2], get_fake())
    #         # object_map[inp] = c_object

    #         new_entry = ei.entry.copy()
    #         new_entry["arguments"] = ei.new_args + ["-c", inp]
    #         new_entry["file"] = os.path.relpath(inp_path, out_dir) if out_dir else inp_path
    #         new_entry["output"] = c_object
    #         del new_entry["type"]
    #         new_entries.append(new_entry)

    for ei in filter(lambda ei: not ei.compile_only, entry_infos):
        new_entry = ei.entry.copy()
        c_objects = [object_map[inp] for inp in ei.c_inputs]
        new_entry["arguments"] = ei.new_args
        # Hacky solution: c2rust-tranpile needs an absolute path here,
        # so we add a path-like prefix so that the transpiler can both
        # parse it correctly and recognize it as a bencoded link command
        new_entry["file"] = "/c2rust/link/" + bencodepy.encode({
            "inputs": c_objects + ei.rest_inputs,  # FIXME: wrong order???
            "libs": ei.libs,
            "lib_dirs": ei.lib_dirs,
            "type": "shared" if ei.shared_lib else "exe",
            # TODO: parse and add in other linker flags
            # for now, we don't do this because rustc doesn't use them
        })
        new_entry["output"] = ei.output or "a.out"
        del new_entry["type"]
        new_entries.append(new_entry)

    return new_entries
