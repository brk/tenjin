import uuid
from dataclasses import dataclass

from dataclasses_json import dataclass_json, DataClassJsonMixin


@dataclass_json
@dataclass
class TransformationRecord:
    name: str
    results_path: str
    start_unix_timestamp: int
    elapsed_ms: int
    exit_code: int
    stderr_lines: list[str] | None
    stdout_lines: list[str] | None


@dataclass
class IngestionRecord(DataClassJsonMixin):  # mixin for better type inference
    uuid: uuid.UUID
    codebase_git_repo_url: str
    codebase_git_commit: str
    codebase_relative_path: str
    tenjin_git_repo_url: str
    tenjin_git_commit: str
    ingest_start_unix_timestamp: int
    ingest_elapsed_ms: int
    guidance: dict
    transformations: list[TransformationRecord]


@dataclass_json
@dataclass
class SubdirectoryFileSnapshot:
    path: str  # relative to the path of the SubdirectorySnapshot
    lines: list[str]
    sha256: str


@dataclass_json
@dataclass
class SubdirectorySnapshot:
    path: str
    files: list[SubdirectoryFileSnapshot]


@dataclass_json
@dataclass
class IngestionResultsSnapshot:
    c_versions: list[SubdirectorySnapshot]
    rust_versions: list[SubdirectorySnapshot]


@dataclass_json
@dataclass
class EndpointEvaluationResults:
    """A given codebase can have multiple endpoints (binaries; functions within libraries;
    binaries wrapping a library function, etc.) that we want to evaluate.

    Each endpoint will have its own input corpus of interest.

    We are primarily concerned with the behavior of a single method within that endpoint,
    """

    target_method: str
    target_file: str

    input_corpus: SubdirectorySnapshot
    input_paths_with_changed_output: list[str]
    input_paths_with_changed_exit_code: list[str]
    inputs_diverged_count: int  # = sum of lengths of above two lists
    inputs_converged_count: int  # = len(input_corpus) - inputs_diverged_count


@dataclass_json
@dataclass
class EvaluationCoverageResult:
    for_ingestion: uuid.UUID

    fuzzing_time_s: int  # how long did it take to generate the input corpus being used?
    exec_time_s: int  # how long did it take to run the target endpoint against the corpus?
    coverage_bitmap: str | None  # base64-encoded bitmap
    # If we take the files in the coverage snapshot and concatenate them in order,
    # the resulting list of lines should be the same length as the (decoded) bitmap.
    coverage_snapshot: SubdirectorySnapshot | None  # files for which coverage is being measured
    coverage_summary_configuration_achieved_lines_count: int
    coverage_summary_configuration_universal_lines_count: int
    coverage_summary_configuration_preprocessor_induced_lines_count: int


@dataclass_json
@dataclass
class EvaluationResults:
    for_ingestion: uuid.UUID
    # It may be of interest to measure coverage both of the Rust and C versions.
    coverage_results: list[EvaluationCoverageResult]
    endpoint_evaluations: list[EndpointEvaluationResults]
