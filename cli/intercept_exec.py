import os
import hashlib
import json
import subprocess
from pathlib import Path


# Integrated functionality from c2rust/scripts/cc-wrappers/common.py
# which does not require Python to be installed outside the hermetic environment.
def intercept_exec(build_type: str, command: str, args: list[str]) -> int:
    build_commands_dir = os.environ.get("BUILD_COMMANDS_DIRECTORY", "/tmp/build_commands")
    # Ensure the build commands directory exists (concurrency-safe)
    Path(build_commands_dir).mkdir(parents=True, exist_ok=True)
    build_info = {
        "type": build_type,
        "directory": os.getcwd(),
        "arguments": [command, *args],
    }
    build_json = json.dumps(build_info, indent=4)

    # Hash the contents of the JSON file and use that as the file name
    # This is safe for concurrency, and guarantees that each unique
    # compilation gets an output file
    hm = hashlib.sha256()
    hm.update(build_json.encode("utf-8"))
    build_file_name = "%s.json" % hm.hexdigest()
    build_file = os.path.join(build_commands_dir, build_file_name)
    with open(build_file, "w", encoding="utf-8") as f:
        f.write(build_json)

    script_dir = os.path.dirname(os.path.realpath(__file__))
    b_arg = ["-B" + script_dir] if build_type == "cc" else []
    return subprocess.call([command, *b_arg, *args])
