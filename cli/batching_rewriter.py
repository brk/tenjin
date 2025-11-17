class BatchingRewriter:
    """
    Context manager for batching multiple rewrites to multiple files.
    Each rewrite is a tuple: (filepath, offset, length, replacement_text).
    Rewrites for each file are applied in descending offset order to avoid offset shifting issues.
    Overlapping rewrites are handled by applying the later rewrite only to the non-overlapping region.
    """

    def __init__(self):
        self.rewrites: dict[str, list[tuple[int, int, str]]] = {}  # type: ignore
        self.contents_cache: dict[str, bytes] = {}

    def add_rewrite(self, filepath: str, offset: int, length: int, replacement_text: str):
        """Add a rewrite operation for a specific file."""
        if filepath not in self.rewrites:
            self.rewrites[filepath] = []
        self.rewrites[filepath].append((offset, length, replacement_text))

    def get_content(self, filepath: str) -> bytes:
        """Get the current content of a file, using cache if available."""
        if filepath not in self.contents_cache:
            with open(filepath, "rb") as f:
                self.contents_cache[filepath] = f.read()
        return self.contents_cache[filepath]

    def get_rewrites(self) -> dict[str, list[tuple[int, int, str]]]:
        return self.rewrites

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
            unique_file_rewrites = set(file_rewrites)
            # Sort rewrites by descending offset
            sorted_rewrites = sorted(unique_file_rewrites, key=lambda r: r[0], reverse=True)
            # Apply rewrites
            for offset, length, replacement_text in sorted_rewrites:
                if offset < 0 or offset + length > len(content):
                    raise ValueError(
                        f"Rewrite out of bounds: offset={offset}, length={length}, file={filepath}"
                    )
                content = content[:offset] + replacement_text.encode() + content[offset + length :]
            # Write back to file
            with open(filepath, "wb") as f:
                f.write(content)
