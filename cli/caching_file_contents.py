type FilePathStr = str


class CachingFileContents:
    def __init__(self) -> None:
        self.cached_bytes: dict[FilePathStr, bytes] = {}

    def get_bytes(self, filepath: FilePathStr) -> bytes:
        if filepath not in self.cached_bytes:
            with open(filepath, "rb") as f:
                self.cached_bytes[filepath] = f.read()
        return self.cached_bytes[filepath]
