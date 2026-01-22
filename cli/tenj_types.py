type FilePathStr = str
type RelativeFilePathStr = str
type FileContentsStr = str
type PreprocessorDefinition = tuple[str, str | None]
type PerFilePreprocessorDefinitions = dict[str, list[PreprocessorDefinition]]
