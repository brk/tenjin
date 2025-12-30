import hashlib
from pathlib import Path

import covset as ccs
import main


def test_llvm_profdata_to_covset_marks_lines(tmp_path: Path):
    codebase = tmp_path / "codebase"
    codebase.mkdir()

    # 4-line file
    src = codebase / "a.c"
    src.write_text("l1\nl2\nl3\nl4\n", encoding="utf-8")

    llvm_json = {
        "data": [
            {
                "files": [
                    {
                        "filename": "a.c",
                        # cover lines 1 and 3
                        "segments": [
                            # line, col, count, has_count, is_region_entry, is_gap_region
                            [1, 1, 1, True, True, False],
                            [2, 1, 0, True, True, False],
                            [3, 2, 5, True, True, False],
                        ],
                    }
                ]
            }
        ]
    }

    covset = ccs.CovSet(
        ccs.llvm_profdata_to_CovSetDict(llvm_json, codebase_path=codebase, compression="identity")
    )
    assert covset.configs == [[]]
    assert len(covset.files) == 1

    expected_hash = hashlib.sha256(src.read_bytes()).hexdigest()
    assert expected_hash in covset.files

    info = covset.files[expected_hash]
    assert "utf8" in info["filepath"]
    assert info["filepath"]["utf8"] == "a.c"

    bitmap = ccs.decode_bitmap(info["encodedcoverage"])

    # covered lines: 1 and 3
    assert bin(bitmap) == "0b101"


def covset_gen_arg_parse_helper(args: list[str]) -> dict[str, str | list[str]]:
    ns, rest = main.parse_covset_gen_args(args)
    return {
        "target": ns.target,
        "codebase": ns.codebase,
        "resultsdir": ns.resultsdir,
        "output": ns.output,
        "rust": ns.rust,
        "extra_args": rest,
    }


def test_covset_gen_arg_parsing_1():
    assert covset_gen_arg_parse_helper([
        "--target",
        "x86_64-unknown-linux-gnu",
        "--codebase",
        "/path/to/codebase",
        "--resultsdir",
        "/path/to/resultsdir",
        "--output",
        "/path/to/output",
        "--extra-arg1",
        "extra-arg2",
    ]) == {
        "target": "x86_64-unknown-linux-gnu",
        "codebase": "/path/to/codebase",
        "resultsdir": "/path/to/resultsdir",
        "output": "/path/to/output",
        "rust": False,
        "extra_args": ["--extra-arg1", "extra-arg2"],
    }


def test_covset_gen_arg_parsing_2():
    assert covset_gen_arg_parse_helper([
        "--target",
        "T",
        "--codebase",
        "C",
        "--resultsdir",
        "R",
        "--output",
        "O",
        "--",
        "extra-arg2",
    ]) == {
        "target": "T",
        "codebase": "C",
        "resultsdir": "R",
        "output": "O",
        "rust": False,
        "extra_args": ["extra-arg2"],
    }


def test_covset_gen_arg_parsing_oops_3():
    # Including a positional arg before the double dash
    # will give likely-unwanted results.
    assert covset_gen_arg_parse_helper([
        "T",
        "--codebase",
        "C",
        "--resultsdir",
        "R",
        "--output",
        "O",
        "--",
        "extra-arg2",
    ]) == {
        "target": None,
        "codebase": "C",
        "resultsdir": "R",
        "output": "O",
        "rust": False,
        "extra_args": ["T", "--", "extra-arg2"],
    }


def test_covset_gen_arg_parsing_5():
    # Note that the separating double dash is required
    # to pass conflicting arguments.
    assert covset_gen_arg_parse_helper([
        "--codebase",
        "C",
        "--resultsdir",
        "R",
        "--output",
        "O",
        "--rust",
        # missing double dash
        "--codebase",
        "extra",
    ]) == {
        "target": None,
        "codebase": "extra",
        "resultsdir": "R",
        "output": "O",
        "rust": True,
        "extra_args": [],
    }
