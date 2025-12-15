from dataclasses import dataclass
from enum import Enum
from pathlib import Path

import compilation_database
import targets_from_intercept
from intercept_exec import InterceptedCommandInfo

"""
Tenjin must be able to handle codebases that build multiple targets.
In particular, we must be able to handle codebases that build shared
libraries plus an executable that links against those shared libraries.

If we only use `compile_commands.json`, we lose the information about
which object files go into which final targets, which means we cannot
safely perform whole-program analyses. If we try to link every object
file into a single target, we may end up with duplicate symbol errors.

Furthermore, we wish to have the generated Cargo project match the
structure of the C build as closely as possible.

Thus, Tenjin needs to represent build targets in a structured way.

CMake provides a "file API" that can be used to query targets
and their source files, but (1) this is CMake-specific, and (2)
it is not quite granular enough for our purposes. For example,
a given source file might be compiled with different flags
for different targets (e.g., different preprocessor definitions),
but the CMake file API would only list the dependency on the source
file.
"""


class TargetType(Enum):
    SHARED = "shared-library"
    EXECUTABLE = "executable"
    STATIC = "static-library"
    OBJECT = "object-file"

    def extension(self) -> str:
        match self:
            case TargetType.SHARED:
                return ".so"
            case TargetType.EXECUTABLE:
                return ""
            case TargetType.STATIC:
                return ".a"
            case TargetType.OBJECT:
                return ".o"
            case _:
                raise ValueError(f"Unknown TargetType: {self}")


type BuildTargetName = str


@dataclass
class BuildTarget:
    name: BuildTargetName
    type: TargetType
    stem: str


class BuildInfo:
    def __init__(self):
        # self._files: dict[BuildTargetName, list[Path]] = {}
        # self._commands: dict[BuildTargetName, list[list[str]]] = {}
        self._intercepted_commands: list[targets_from_intercept.InterceptedCommand] = []
        self._implicit_target: BuildTarget | None = None
        self._basedir: Path | None = None

    def with_compilation_database(self, compdb: Path, implicit_target: BuildTarget) -> None:
        def to_intercepted(
            cmd: compilation_database.CompileCommand,
        ) -> targets_from_intercept.InterceptedCommand:
            entry: InterceptedCommandInfo = {
                "type": "cc",
                "directory": cmd.directory,
                "arguments": cmd.get_command_parts(),
                "file": cmd.file,
                "output": cmd.output,
            }
            return targets_from_intercept.convert_intercepted_entry(entry)

        ccmds = compilation_database.CompileCommands.from_json_file(compdb)
        cmd_infos = [to_intercepted(c) for c in ccmds.commands]
        self.set_intercepted_commands(cmd_infos, implicit_target, compdb.parent)

    def set_intercepted_commands(
        self,
        intercepted_commands: list[targets_from_intercept.InterceptedCommand],
        implicit_target: BuildTarget | None,
        builddir: Path,
    ):
        self._intercepted_commands = intercepted_commands
        self._implicit_target = implicit_target
        self._basedir = builddir

        d = self._process_targets()
        import pprint

        pprint.pprint(d)

    def _process_targets(
        self,
    ) -> dict[BuildTargetName, tuple[BuildTarget, list[targets_from_intercept.InterceptedCommand]]]:
        """Returns a mapping from target names to (BuildTarget, list of commands).

        The list of commands includes both compilation and linking commands."""
        if self._implicit_target is not None:
            # When an implicit target is given, we assume all commands
            # belong to that target.
            return {self._implicit_target.name: (self._implicit_target, self._intercepted_commands)}

        # Without an implicit target, we must try to reconstruct
        # the targets from the intercepted commands.

        # Sanity check: ensure no duplicate object file outputs
        c_outputs_list = [
            c.output for c in self._intercepted_commands if c.compile_only and c.output is not None
        ]
        assert len(c_outputs_list) == len(set(c_outputs_list)), (
            "Duplicate object file outputs detected"
        )

        # Sanity check: ensure no duplicate target names
        def cmd_target_name(link_cmd: targets_from_intercept.InterceptedCommand) -> str:
            return Path(link_cmd.output).name if link_cmd.output else "unknown"

        target_names = [
            cmd_target_name(c) for c in self._intercepted_commands if not c.compile_only
        ]
        assert len(target_names) == len(set(target_names)), "Duplicate target names detected"

        intermediates: dict[str, targets_from_intercept.InterceptedCommand] = {}
        for c in self._intercepted_commands:
            if c.compile_only:
                assert c.output is not None
                intermediates[c.output] = c

        link_commands = list(filter(lambda c: not c.compile_only, self._intercepted_commands))

        targets: dict[str, BuildTarget] = {}
        for link_cmd in link_commands:
            target_name = cmd_target_name(link_cmd)
            target_type = (
                TargetType.SHARED
                if any("-shared" in arg for arg in link_cmd.entry["arguments"])
                else TargetType.EXECUTABLE
            )
            target_stem = Path(target_name).stem
            target = BuildTarget(name=target_name, type=target_type, stem=target_stem)
            targets[target_name] = target

            import pprint

            pprint.pprint("link_cmd:")
            pprint.pprint(link_cmd)

        target_to_cmds = {}
        for link_cmd in link_commands:
            target = targets[cmd_target_name(link_cmd)]
            target_cmds = [link_cmd]
            for intermediate in link_cmd.rest_inputs:
                if intermediate in intermediates:
                    c_cmd = intermediates[intermediate]
                    target_cmds.append(c_cmd)
            target_to_cmds[target.name] = (target, target_cmds)

        return target_to_cmds

    # def targets(self) -> list[BuildTarget]:
    #     """Return the list of build targets."""
    #     return self._targets

    # def source_files_for_target(self, target: BuildTarget) -> list[Path]:
    #     """Return the list of source files for the given target."""
    #     return self._files.get(target.name, [])

    # def commands_for_target(self, target: BuildTarget) -> list[list[str]]:
    #     """Return the list of build commands for the given target."""
    #     return self._commands.get(target.name, [])
