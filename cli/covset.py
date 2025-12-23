import sys
import json
import base64
import zlib
import bz2
from pathlib import Path
from typing import TypedDict, Union, Optional, Literal, cast
from typing_extensions import NotRequired

"""
Type definitions for the Coverage Set (covset) format.

A coverage set represents code coverage data for a C codebase. It stores
which lines of source code have been executed during testing, along with
configuration information about how files were compiled.

Format Overview
---------------
A covset is a JSON document with two top-level keys:

- "files": A mapping from file content hashes to file coverage information
- "configs": An array of unique configuration arrays (for deduplication)

The coverage data for each file is stored as a bitmap where each bit
corresponds to a source line (bit=1 means covered, bit=0 means not covered).
"""

type CompressionType = Literal["identity", "zlib", "zstd"]


class EncodedCoverage(TypedDict):
    """
    Encoded and optionally compressed coverage bitmap (for a single file).

    The bitmap represents line coverage where each bit corresponds to a source
    line.

    Attributes
    ----------
    b64 : str
        Base64-encoded binary data. After decoding and decompressing,
        this yields a bitmap where bit N indicates whether line N is covered.
    compression : str
        Compression algorithm: "identity", "zlib", or "bzip2".
        Defaults to "identity" if not present.
    """

    b64: str
    compression: NotRequired[CompressionType]


class FilePath(TypedDict, total=False):
    """
    Exactly one of 'hex' or 'utf8' should be non-null.

    Attributes
    ----------
    hex : str | None
        Hex-encoded file path bytes (for non-UTF8 paths).
    utf8 : str | None
        UTF-8 encoded file path string (most common case).
    """

    hex: Optional[str]
    utf8: Optional[str]


ConfigReference = Union[int, list[str]]
"""
Reference to a configuration.

Can be either:
- int: An index into the top-level "configs" array (for deduplication)
- list[str]: An inline array of configuration strings

The configuration strings typically represent preprocessor definitions
and compiler options used when building the file.
"""


class FileInfo(TypedDict):
    """
    Coverage and metadata for a single source file.

    Attributes
    ----------
    filepath : FilePath
        Path to the source file, typically relative to the codebase root.

    expandedhash : str | None
        SHA-256 hash of the preprocessed/expanded file contents.
        Used to detect when the same source file produces different
        results under different configurations. May be null if not computed.

    encodedcoverage : EncodedCoverage
        The coverage bitmap for this file, encoded and compressed.

    encodedcoverable : EncodedCoverage
        Encoded and compressed bitmap indicating which lines are coverable
        (by traditional execution coverage metrics).

    misc : dict
        Optional arbitrary JSON value for extension data.
    """

    filepath: FilePath
    expandedhash: Optional[str]
    encodedcoverage: EncodedCoverage
    encodedcoverable: EncodedCoverage
    misc: dict


# Note: TypedDict doesn't support keys with hyphens directly in class syntax.
# The actual keys "config-inputs" and "config-outputs" must be accessed via
# subscript notation: file_info["config-inputs"]

type Sha256Hex = str
type FilesDict = dict[Sha256Hex, FileInfo]
"""
Mapping from file content hashes to file information.

Keys are lowercase hex-encoded SHA-256 hashes of the original source file
contents (before preprocessing). This allows identifying files by content
rather than by path, which is useful when the same file might exist at
different paths or when paths change.
"""


type ConfigsArray = list[list[str]]
"""
Array of unique configuration arrays.

Each inner array is a list of configuration strings (preprocessor definitions,
compiler flags, etc.). The outer array has no duplicate inner arrays.

This structure allows FileInfo entries to reference configurations by index
rather than repeating the full configuration array, saving space when many
files share the same configuration.

Example:
    [
        ["-DDEBUG", "-O0"],
        ["-DNDEBUG", "-O2"],
        ["-DTEST", "-DDEBUG"]
    ]
"""


class CovSetDict(TypedDict):
    """
    Top-level coverage set document structure.

    A covset represents code coverage data for a codebase, storing:
    - Which source files were covered
    - Which lines in each file were executed
    - Configuration information for each file

    Attributes
    ----------
    files : FilesDict
        Mapping from file content SHA-256 hashes to coverage information.
        Each entry contains the coverage bitmap and metadata for one file.

    configs : ConfigsArray
        Array of unique configuration arrays. FileInfo entries can reference
        these by index to avoid duplicating configuration strings.

    Example
    -------
    {
        "files": {
            "abc123...": {
                "filepath": {"utf8": "src/main.c", "hex": null},
                "expandedhash": "def456...",
                "config-inputs": 0,
                "config-outputs": 0,
                "encodedcoverage": {
                    "b64": "eJxz...",
                    "compression": "zlib"
                }
            }
        },
        "configs": [
            ["-DDEBUG", "-I/usr/include"]
        ]
    }
    """

    files: FilesDict
    configs: ConfigsArray


type SetOperation = Literal["union", "intersection", "difference", "symmetric_diff", "negate"]

type MismatchPolicy = Literal["error", "warn", "ignore"]
"""
Policy for handling expanded hash mismatches during set operations.

When combining coverage from two sets, files may have different expanded
hashes (indicating different preprocessing results). This policy controls
the behavior:

- "error": Raise an exception (default, safest)
- "warn": Print a warning but continue
- "ignore": Silently continue
"""


class CovSet:
    """Represents a coverage set and provides methods for loading and saving."""

    def __init__(self, data: CovSetDict):
        """Initializes a CovSet object from a dictionary."""
        if "files" not in data or "configs" not in data:
            raise ValueError("Invalid covset format: missing 'files' or 'configs' key.")
        self.files = data["files"]
        self.configs = data["configs"]

    def to_json_dict(self) -> CovSetDict:
        """Returns the covset data as a dictionary."""
        return {"files": self.files, "configs": self.configs}

    @staticmethod
    def load(filepath: str) -> "CovSet":
        """Loads a covset from a JSON file."""
        with open(filepath, "r", encoding="utf-8") as f:
            return CovSet(json.load(f))

    def save(self, filepath: str):
        """Saves a covset to a JSON file."""
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(self.to_json_dict(), f, indent=2)


def decode_bitmap(encoded_data: EncodedCoverage) -> int:
    """Decodes and decompresses bitmap data from the covset format."""
    compression = encoded_data.get("compression", "identity")
    b64_data = encoded_data["b64"]

    binary_data = base64.b64decode(b64_data)

    if compression == "identity":
        return _bytes_to_bits(binary_data)
    elif compression == "zlib":
        return _bytes_to_bits(zlib.decompress(binary_data))
    elif compression == "bzip2":
        return _bytes_to_bits(bz2.decompress(binary_data))
    else:
        raise ValueError(f"Unknown compression type: {compression}")


def encode_bitmap(bitmap: int, compression: CompressionType) -> EncodedCoverage:
    """Compresses and encodes bitmap data into the covset format."""
    binary_data = _bits_to_bytes(bitmap)
    if compression == "identity":
        compressed_data = binary_data
    elif compression == "zlib":
        compressed_data = zlib.compress(binary_data)
    elif compression == "bzip2":
        compressed_data = bz2.compress(binary_data)
    else:
        raise ValueError(f"Unknown compression type: {compression}")

    b64_data = base64.b64encode(compressed_data).decode("ascii")
    return {"b64": b64_data, "compression": compression}


type Sexpr = Union[str, list["Sexpr"]]


def parse_sexp(s: str) -> Sexpr:
    """
    Parses a simple s-expression string into a nested list structure.
    Handles quoted strings as atoms.
    """
    s = s.strip()
    if not s.startswith("(") or not s.endswith(")"):
        # It's an atom
        return s.strip('"')

    # It's an expression
    s = s[1:-1].strip()

    parts = []
    balance = 0
    current_part = ""
    in_string = False

    for char in s:
        if char == '"':
            in_string = not in_string

        if char == "(" and not in_string:
            balance += 1
        elif char == ")" and not in_string:
            balance -= 1

        if char.isspace() and balance == 0 and not in_string:
            if current_part:
                parts.append(parse_sexp(current_part))
                current_part = ""
        else:
            current_part += char

    if current_part:
        parts.append(parse_sexp(current_part))

    return parts


def _merge_configs(
    configs1: ConfigsArray, configs2: ConfigsArray
) -> tuple[ConfigsArray, dict[int, int], dict[int, int]]:
    """
    Merges two config lists, creating a unified list and mappings
    from the old config indices to the new ones.
    """
    conf1_tuples = [tuple(c) for c in configs1]
    conf2_tuples = [tuple(c) for c in configs2]

    # Create a sorted list of unique configs
    new_configs_tuples = sorted(list(set(conf1_tuples) | set(conf2_tuples)))

    new_configs = [list(c) for c in new_configs_tuples]

    # Create mappings from old indices to new indices
    map1 = {i: new_configs_tuples.index(conf) for i, conf in enumerate(conf1_tuples)}
    map2 = {i: new_configs_tuples.index(conf) for i, conf in enumerate(conf2_tuples)}

    return new_configs, map1, map2


def _remap_file_info_configs(file_info, mapping):
    """Remaps config indices in a file_info dict using a provided mapping."""
    new_info = file_info.copy()
    for key in ["config-inputs", "config-outputs"]:
        if key in new_info and isinstance(new_info[key], int):
            new_info[key] = mapping[new_info[key]]
    return new_info


def _bitwise_op(int1: int, int2: int, op: SetOperation) -> int:
    """
    Performs a bitwise operation (union, intersection, etc.) on two bitmaps.
    """
    if op == "union":
        result_int = int1 | int2
    elif op == "intersection":
        result_int = int1 & int2
    elif op == "difference":
        result_int = int1 & ~int2
    elif op == "symmetric_diff":
        result_int = int1 ^ int2
    else:
        raise ValueError(f"Unknown bitwise operation: {op}")

    return result_int


def op_negate(covset: CovSet, compression: CompressionType) -> CovSet:
    """Performs a bitwise NOT on all file bitmaps in a covset."""
    new_files = {}
    for fhash, info in covset.files.items():
        new_info = info.copy()
        bitmap = decode_bitmap(info["encodedcoverage"])

        if not bitmap:
            new_info["encodedcoverage"] = encode_bitmap(0, compression)
            new_files[fhash] = new_info
            continue

        bit_length = bitmap.bit_length()
        mask = (1 << bit_length) - 1

        negated_int = ~bitmap & mask

        new_info["encodedcoverage"] = encode_bitmap(negated_int, compression)
        new_files[fhash] = new_info

    return CovSet({"files": new_files, "configs": covset.configs})


def set_operation(
    op: SetOperation,
    set1: CovSet,
    set2: CovSet,
    mismatch_policy: MismatchPolicy,
    compression: CompressionType,
) -> CovSet:
    """
    Performs a binary set operation on two CovSets.
    """
    new_configs, map1, map2 = _merge_configs(set1.configs, set2.configs)
    new_files = {}

    all_file_hashes = set(set1.files.keys()) | set(set2.files.keys())

    for fhash in all_file_hashes:
        in1 = fhash in set1.files
        in2 = fhash in set2.files

        info1 = set1.files.get(fhash)
        info2 = set2.files.get(fhash)

        if in1 and in2:
            assert info1 is not None and info2 is not None
            exp_hash1 = info1.get("expandedhash")
            exp_hash2 = info2.get("expandedhash")
            if exp_hash1 and exp_hash2 and exp_hash1 != exp_hash2:
                msg = f"Expanded hash mismatch for file hash {fhash[:10]}..."
                if mismatch_policy == "error":
                    raise ValueError(msg)
                elif mismatch_policy == "warn":
                    print(f"Warning: {msg}", file=sys.stderr)

        base_info = info1 if in1 else info2
        new_info = _remap_file_info_configs(base_info, map1 if in1 else map2)

        # Perform bitmap operation based on the set operation type
        if op in ("union", "symmetric_diff"):
            if in1 and in2:
                assert info1 is not None and info2 is not None
                bitmap1 = decode_bitmap(info1["encodedcoverage"])
                bitmap2 = decode_bitmap(info2["encodedcoverage"])
                new_bitmap = _bitwise_op(bitmap1, bitmap2, op)
                new_info["encodedcoverage"] = encode_bitmap(new_bitmap, compression)
            elif in1:
                assert info1 is not None
                new_info["encodedcoverage"] = info1["encodedcoverage"]
            else:  # in2
                assert info2 is not None
                new_info["encodedcoverage"] = info2["encodedcoverage"]
            new_files[fhash] = new_info

        elif op == "intersection":
            if in1 and in2:
                assert info1 is not None and info2 is not None
                bitmap1 = decode_bitmap(info1["encodedcoverage"])
                bitmap2 = decode_bitmap(info2["encodedcoverage"])
                new_bitmap = _bitwise_op(bitmap1, bitmap2, "intersection")
                new_info["encodedcoverage"] = encode_bitmap(new_bitmap, compression)
                new_files[fhash] = new_info

        elif op == "difference":
            if in1 and not in2:
                assert info1 is not None
                new_info["encodedcoverage"] = info1["encodedcoverage"]
                new_files[fhash] = new_info
            elif in1 and in2:
                assert info1 is not None and info2 is not None
                bitmap1 = decode_bitmap(info1["encodedcoverage"])
                bitmap2 = decode_bitmap(info2["encodedcoverage"])
                new_bitmap = _bitwise_op(bitmap1, bitmap2, "difference")
                new_info["encodedcoverage"] = encode_bitmap(new_bitmap, compression)
                new_files[fhash] = new_info

    return CovSet({"files": new_files, "configs": new_configs})


def evaluate_exp(
    exp: Sexpr, mismatch_policy: MismatchPolicy, compression: CompressionType
) -> CovSet:
    """Recursively evaluates a parsed s-expression."""
    if not isinstance(exp, list):
        # Atom: a filepath
        return CovSet.load(exp)

    assert isinstance(exp, list) and len(exp) >= 1 and isinstance(exp[0], str)
    op = exp[0].lower()

    # Unary operations
    if op == "negate":
        if len(exp) != 2:
            raise ValueError(f"Operation '{op}' requires 1 argument, got {len(exp) - 1}")
        covset = evaluate_exp(exp[1], mismatch_policy, compression)
        return op_negate(covset, compression)

    if op == "show":
        if len(exp) != 2:
            raise ValueError(f"Operation '{op}' requires 1 argument, got {len(exp) - 1}")
        covset = evaluate_exp(exp[1], mismatch_policy, compression)
        return op_show(covset)

    # Binary operations
    binary_ops = {"union", "intersection", "difference", "symmetric_diff"}
    if op in binary_ops:
        if len(exp) != 3:
            raise ValueError(f"Operation '{op}' requires 2 arguments, got {len(exp) - 1}")

        set1 = evaluate_exp(exp[1], mismatch_policy, compression)
        set2 = evaluate_exp(exp[2], mismatch_policy, compression)

        op = cast(SetOperation, op)
        return set_operation(op, set1, set2, mismatch_policy, compression)

    raise ValueError(f"Unknown operation: {op}")


def op_show(covset: CovSet) -> CovSet:
    """Shows coverage for all files in the covset."""
    show(covset.to_json_dict(), Path("."))
    return covset


def show(covset_data: CovSetDict, codebase_path: Path):
    """
    Prints each source file with lines prefixed by '+' (covered) or '-' (not covered).
    """
    files_to_visualize = covset_data.get("files", {})

    if not codebase_path.is_dir():
        raise FileNotFoundError(f"Codebase path not found or not a directory: {codebase_path}")

    for fhash, file_info in files_to_visualize.items():
        filepath_info = file_info.get("filepath")
        if not filepath_info:
            print(f"Warning: Missing filepath for hash {fhash[:10]}...", file=sys.stderr)
            continue

        relative_path = filepath_info.get("utf8")
        if not relative_path:
            print(f"Warning: No utf8 filepath for hash {fhash[:10]}...", file=sys.stderr)
            continue

        source_file_path = codebase_path / relative_path

        if not source_file_path.is_file():
            print(f"Warning: Source file not found: {source_file_path}", file=sys.stderr)
            continue

        print("-" * 40)
        print(f"File: {relative_path}")
        print("-" * 40)

        try:
            covered = decode_bitmap(file_info["encodedcoverage"])
            coverable = decode_bitmap(file_info["encodedcoverable"])

            with open(source_file_path, "r", encoding="utf-8") as f:
                for i, line in enumerate(f):
                    is_covered = (covered & (1 << i)) != 0
                    is_coverable = (coverable & (1 << i)) != 0
                    if is_coverable:
                        prefix = "+" if is_covered else "-"
                    else:
                        prefix = " "
                    print(f"{prefix} {line.rstrip()}")

        except Exception as e:
            print(f"Error processing file {relative_path}: {e}", file=sys.stderr)

        print()

    covered_lines = 0
    coverable_lines = 0
    for fhash, file_info in files_to_visualize.items():
        covered = decode_bitmap(file_info["encodedcoverage"])
        coverable = decode_bitmap(file_info["encodedcoverable"])
        covered_lines += covered.bit_count()
        coverable_lines += coverable.bit_count()
    print("=" * 40)
    coverage_percentage = (covered_lines / coverable_lines) if coverable_lines > 0 else 0
    print(f"Total covered lines: {covered_lines} / {coverable_lines} = {coverage_percentage:.2%}")


def _bytes_to_bits(b: bytes) -> int:
    return int.from_bytes(b, "little")


def _bits_to_bytes(bits: int) -> bytes:
    bytes_needed = (bits.bit_length() + 7) // 8
    return bits.to_bytes(bytes_needed, "little")


def do_eval(
    output: Optional[str],
    expression: str,
    compression: CompressionType,
    on_mismatch: MismatchPolicy = "error",
):
    try:
        parsed_exp = parse_sexp(expression)
        result_covset = evaluate_exp(parsed_exp, on_mismatch, compression)

        output_json = json.dumps(result_covset.to_json_dict(), indent=2)

        if output:
            with open(output, "w", encoding="utf-8") as f:
                f.write(output_json)
            print(f"Result written to {output}")
        else:
            print(output_json)

    except (ValueError, FileNotFoundError, json.JSONDecodeError, TypeError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
