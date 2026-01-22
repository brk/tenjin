from caching_file_contents import CachingFileContents
from tenj_types import FilePathStr


class BatchingRewriter:
    """
    Context manager for batching multiple rewrites to multiple files.
    Each rewrite is a tuple: (filepath, offset, length, replacement_text).
    Rewrites for each file are applied in descending offset order to avoid offset shifting issues.
    Overlapping rewrites are handled by applying the later rewrite only to the non-overlapping region.
    """

    def __init__(self) -> None:
        self.rewrites: dict[FilePathStr, list[tuple[int, int, str]]] = {}  # type: ignore
        self.contents_cache = CachingFileContents()

    def add_rewrite(self, filepath: FilePathStr, offset: int, length: int, replacement_text: str):
        """Add a rewrite operation for a specific file."""
        if filepath not in self.rewrites:
            self.rewrites[filepath] = []
        self.rewrites[filepath].append((offset, length, replacement_text))

    def get_content(self, filepath: FilePathStr) -> bytes:
        """Get the current content of a file."""
        return self.contents_cache.get_bytes(filepath)

    def get_rewrites(self, reverse: bool = True) -> dict[str, list[tuple[int, int, str]]]:
        return {k: sorted_file_rewrites(v, reverse=reverse) for k, v in self.rewrites.items()}

    def replace_rewrites(self, rewrites: dict[str, list[tuple[int, int, str]]]):
        self.rewrites = rewrites

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is not None:
            return False  # propagate exception

        self.apply_rewrites()

    def apply_rewrites(self):
        if not self.rewrites:
            return
        for filepath, file_rewrites in self.rewrites.items():
            # Read file contents
            with open(filepath, "rb") as f:
                content = f.read()
            # Apply rewrites
            sorted_rewrites = sorted_file_rewrites(file_rewrites)
            for offset, length, replacement_text in sorted_rewrites:
                if offset < 0 or offset + length > len(content):
                    raise ValueError(
                        f"Rewrite out of bounds: offset={offset}, length={length}, file={filepath}"
                    )
                content = content[:offset] + replacement_text.encode() + content[offset + length :]
            # Write back to file
            with open(filepath, "wb") as f:
                f.write(content)

    def capture_snapshot(self) -> dict[str, bytes]:
        """Capture a snapshot of the current contents of all files involved in rewrites
        (without any pending rewrites applied)."""
        snapshot = {}
        for filepath in self.rewrites.keys():
            snapshot[filepath] = self.get_content(filepath)
        return snapshot

    def restore_snapshot(self, snapshot: dict[str, bytes]):
        """Restore the contents of files from a previously captured snapshot."""
        for filepath, content in snapshot.items():
            with open(filepath, "wb") as f:
                f.write(content)


def sorted_file_rewrites(
    file_rewrites: list[tuple[int, int, str]], reverse: bool = True
) -> list[tuple[int, int, str]]:
    unique_file_rewrites = set(file_rewrites)
    # Sort rewrites by descending offset
    return sorted(unique_file_rewrites, key=lambda r: r[0], reverse=reverse)
