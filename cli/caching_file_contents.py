type FilePathStr = str


class CachingFileContents:
    def __init__(self):
        self.cached_bytes: dict[FilePathStr, bytes] = {}
        # self.cached_str: dict[FilePathStr, str] = {}

    def get_bytes(self, filepath: FilePathStr) -> bytes:
        if filepath not in self.cached_bytes:
            with open(filepath, "rb") as f:
                self.cached_bytes[filepath] = f.read()
                # self.cached_str[filepath] = self.cached_bytes[filepath].decode("utf-8")
        return self.cached_bytes[filepath]
