import hashlib
from pathlib import Path

import covset as ccs


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
