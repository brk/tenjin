from pathlib import Path
import os
from sys import argv


def localdir() -> Path:
    return find_repo_root_dir_Path() / "_local"


def find_repo_root_dir_Path(start_dir=None) -> Path:
    def validate_candidate_dir(p: Path):
        return (p / "cli" / "sh" / "provision.sh").is_file()

    if not start_dir:
        # Try to short-circuit the filesystem walk by using the path
        # to the currently executing script.
        tenjdir = Path(argv[0]).resolve().parent
        rootdir = tenjdir.parent
        if validate_candidate_dir(rootdir):
            return rootdir
        else:
            start_dir = os.getcwd()

    p = Path(start_dir)
    while True:
        if validate_candidate_dir(p):
            return p

        # Keep going up until we hit the filesystem root
        if p == p.parent:
            raise FileNotFoundError("Could not find the repo root directory.")

        p = p.parent
