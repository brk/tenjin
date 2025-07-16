import enum
import shutil
import tempfile
from pathlib import Path
from subprocess import CompletedProcess, CalledProcessError
import re
import os
import json
import pprint
import graphlib
from typing import Callable, Sequence
import time
import uuid
import shlex
from platform import platform
import hashlib

import click

from repo_root import find_repo_root_dir_Path, localdir
import provisioning
import ingest
import ingest_tracking
import hermetic
import vcs_helpers
import static_measurements_rust
from speculative_rewriters import (
    CondensedSpanGraph,
    ExplicitSpan,
    SpeculativeFileRewriter,
    SpeculativeSpansEraser,
)


def stub_ingestion_record(codebase: Path, guidance: dict) -> ingest.TranslationRecord | None:
    """
    Create a stub IngestionRecord for the given codebase and crate name.
    This is used to initialize the ingestion record before the actual translation.
    """

    codebase_vcs_dir = vcs_helpers.find_containing_vcs_dir(codebase)
    if codebase_vcs_dir is None:
        return None

    codebase_wcs = vcs_helpers.vcs_working_copy_status(codebase_vcs_dir)

    tenjin_vcs_dir = vcs_helpers.find_containing_vcs_dir(find_repo_root_dir_Path())
    assert tenjin_vcs_dir is not None, "No VCS directory found for Tenjin?!?!"

    tenjin_wcs = vcs_helpers.vcs_working_copy_status(tenjin_vcs_dir)

    assert tenjin_wcs.origin is not None, "Tenjin working copy has no origin URL?!?"
    assert tenjin_wcs.commit is not None, "Tenjin working copy has no commit hash?!?"

    assert codebase_wcs.origin is not None, "Codebase working copy has no origin URL?!?"
    assert codebase_wcs.commit is not None, "Codebase working copy has no commit hash?!?"

    codebase_relative_path = codebase.relative_to(vcs_helpers.vcs_root(codebase_vcs_dir))
    upstream_c2rust = provisioning.HAVE.query("10j-reference-c2rust-tag")
    return ingest.TranslationRecord(
        translation_uuid=uuid.uuid4(),
        inputs=ingest.TranslationInputs(
            codebase=ingest.IngestedCodebase(
                git_repo_url=codebase_wcs.origin,
                git_commit=codebase_wcs.commit,
                relative_path=str(codebase_relative_path),
            ),
            host_platform=platform(),
            tenjin_git_repo_url=tenjin_wcs.origin,
            tenjin_git_commit=tenjin_wcs.commit,
            c2rust_baseline_version=upstream_c2rust or "unknown",
            per_file_preprocessor_definitions={},
            guidance=guidance,
        ),
        results=ingest.TranslationResults(
            translation_start_unix_timestamp=int(time.time()),
            translation_elapsed_ms=0,
            static_measurement_elapsed_ms=0,
            transformations=[],
            c2rust_baseline=None,
            tenjin_initial=None,
            tenjin_final=None,
        ),
    )


def run_c2rust(
    tracker: ingest_tracking.TimingRepo,
    tag: str,
    c2rust_bin: Path,
    compdb: Path,
    output: Path,
    flags: list[str],
) -> CompletedProcess:
    with tracker.tracking(tag, output) as step:
        cp = hermetic.run(
            [
                str(c2rust_bin),
                "transpile",
                str(compdb),
                "-o",
                str(output),
                *flags,
            ],
            check=True,
            env_ext={
                "RUST_BACKTRACE": "1",
            },
            capture_output=True,
        )
        step.update_sub(cp)
        return cp


def create_subdirectory_snapshot(
    is_rust: bool,
    codebase: Path,
    subdir_label: str,
) -> ingest.SubdirectorySnapshot:
    def snapshot_for_file(p: Path) -> ingest.SubdirectoryFileSnapshot:
        content_bytes = p.read_bytes()
        lines = content_bytes.decode("utf-8", errors="replace").splitlines()
        relpath = p.relative_to(codebase).as_posix()
        if relpath == ".":
            relpath = p.name
        return ingest.SubdirectoryFileSnapshot(
            path=relpath,
            lines=lines,
            sha256=hashlib.sha256(content_bytes).hexdigest(),
        )

    if codebase.is_file():
        # If the codebase is a single file, we treat it as a subdirectory with one file.
        return ingest.SubdirectorySnapshot(
            path=subdir_label,
            files=[snapshot_for_file(codebase)],
        )

    file_snapshots = []
    paths = sorted(list(codebase.rglob("*")))
    for p in paths:
        if p.is_file():
            if p.suffix not in [".json", ".rs", ".c", ".h"]:
                continue
            if not is_rust and "CMakeFiles" in p.parts:
                continue
            if is_rust and p.relative_to(codebase).parts[0] == "target":
                continue
            file_snapshots.append(snapshot_for_file(p))
    return ingest.SubdirectorySnapshot(
        path=subdir_label,
        files=file_snapshots,
    )


def create_translation_snapshot(
    root: Path, codebase: Path, resultsdir: Path, record: ingest.TranslationRecord
) -> ingest.TranslationResultsSnapshot:
    c_snapshot = create_subdirectory_snapshot(False, codebase, "original_codebase")

    rust_snapshots = []
    for dirname in ["vanilla_c2rust", "00_out", "final"]:
        subdir = resultsdir / dirname
        assert subdir.is_dir()
        rust_snapshots.append(create_subdirectory_snapshot(True, subdir, dirname))

    results_snapshot = ingest.TranslationResultsSnapshot(
        for_translation=record.translation_uuid,
        c_versions=[c_snapshot],
        rust_versions=rust_snapshots,
    )

    return results_snapshot


def write_synthetic_compile_commands_to(compdb_path: Path, c_file: Path, builddir: Path):
    """Write a synthetic compile_commands.json file for a single C file."""
    assert compdb_path.parent.is_dir()
    outname = c_file.with_suffix(".o").name
    cc = hermetic.xj_llvm_root(localdir()) / "bin" / "clang"
    c_file_full_q = shlex.quote(c_file.resolve().as_posix())
    contents = json.dumps(
        [
            {
                "directory": builddir.as_posix(),
                "command": f"{cc} -c {c_file_full_q} -o {shlex.quote(outname)}",
                "file": c_file.resolve().as_posix(),
                "output": outname,
            }
        ],
        indent=2,
    )
    compdb_path.write_text(contents, encoding="utf-8")


def do_translate(
    root: Path,
    codebase: Path,
    resultsdir: Path,
    cratename: str,
    guidance_path_or_literal: str,
    c_main_in: str | None = None,
):
    """
    Translate a codebase from C to Rust.

    The `codebase` argument should be a path to a directory or a single C file.

    The input directory should have a pre-generated compile_commands.json file,
    or an easy means of generating one (such as a CMakeLists.txt file).

    The `resultsdir` directory will contain subdirectories with intermediate
    stages of the resulting translation. Assuming no errors occurred, the final
    translation will be in the `final` subdirectory. The `resultsdir` will also
    have files called `translation_metadata.json` and `translation_snapshot.json`.
    """

    guidance = load_and_parse_guidance(guidance_path_or_literal)

    tracker = ingest_tracking.TimingRepo(stub_ingestion_record(codebase, guidance))

    def perform_pre_translation(builddir: Path) -> Path:
        """Returns the path to the provided-or-generated compile_commands.json file."""
        provided_compdb = codebase / "compile_commands.json"
        provided_cmakelists = codebase / "CMakeLists.txt"

        if provided_cmakelists.exists() and not provided_compdb.exists():
            # If we have a CMakeLists.txt, we can generate the compile_commands.json
            cp = hermetic.run(
                [
                    "cmake",
                    "-S",
                    str(codebase),
                    "-B",
                    str(builddir),
                    "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
                ],
                check=True,
                capture_output=True,
            )
            tracker.update_sub(cp)
            return builddir / "compile_commands.json"
        elif codebase.is_file() and codebase.suffix == ".c":
            # If we have a single C file, we can trivially generate a compile_commands.json
            # with a single entry for it.
            write_synthetic_compile_commands_to(
                builddir / "compile_commands.json", codebase, builddir
            )
            return builddir / "compile_commands.json"
        else:
            # Otherwise, we assume the compile_commands.json is already present
            return provided_compdb

    c2rust_transpile_flags = [
        "--translate-const-macros",
        "--reduce-type-annotations",
        "--disable-refactoring",
    ]

    if not c_main_in and codebase.is_file() and codebase.suffix == ".c":
        c_main_in = codebase.name

    if not c_main_in and (codebase / "main.c").is_file():
        c_main_in = "main.c"

    if not c_main_in and (codebase / "src" / "main.c").is_file():
        c_main_in = "main.c"

    if c_main_in:
        c2rust_transpile_flags.extend(["--binary", c_main_in.removesuffix(".c")])

    xj_c2rust_transpile_flags = [
        *c2rust_transpile_flags,
        "--log-level",
        "INFO",
        "--guidance",
        json.dumps(guidance),
    ]

    with tempfile.TemporaryDirectory() as builddirname:
        builddir = Path(builddirname)
        with tracker.tracking("pretranslation", builddir) as _step:
            compdb = perform_pre_translation(builddir)

        with open(compdb, "r", encoding="utf-8") as compdb_f:
            tracker.set_preprocessor_definitions(
                extract_preprocessor_definitions_from_compile_commands(
                    json.load(compdb_f),
                    codebase,
                ),
            )

        # The crate name that c2rust uses is based on the directory stem,
        # so we create a subdirectory with the desired crate name.
        output = resultsdir / cratename
        output.mkdir(parents=True, exist_ok=False)
        # First run the upstream c2rust tool to get a baseline translation.
        upstream_c2rust_bin = localdir() / "upstream-c2rust" / "target" / "debug" / "c2rust"
        _up_cp = run_c2rust(
            tracker, "upstream-c2rust", upstream_c2rust_bin, compdb, output, c2rust_transpile_flags
        )
        output = output.rename(output.with_name("vanilla_c2rust"))

        # Then run our version, using guidance and preanalysis.
        output = resultsdir / cratename
        output.mkdir(parents=True, exist_ok=False)
        c2rust_bin = root / "c2rust" / "target" / "debug" / "c2rust"
        try:
            _xj_cp = run_c2rust(
                tracker, "xj-c2rust", c2rust_bin, compdb, output, xj_c2rust_transpile_flags
            )
        except CalledProcessError as e:
            click.echo("stdout:", err=True)
            click.echo(e.stdout.decode("utf-8", errors="replace"), err=True)
            click.echo("stderr:", err=True)
            click.echo(e.stderr.decode("utf-8", errors="replace"), err=True)
            raise

        # Normalize the unmodified translation results to end up
        # in a directory with a project-independent name.
        output = output.rename(output.with_name("00_out"))

    # Verify that the initial translation is valid Rust code.
    # If it has errors, we won't be able to run the improvement passes.
    initial_cp = hermetic.run_cargo_in(["check"], cwd=output, check=False)
    # Ensure that subsequent passes start with a clean slate.
    hermetic.run_cargo_in(["clean", "-p", cratename], cwd=output, check=True)

    if initial_cp.returncode == 0:
        run_improvement_passes(root, output, resultsdir, cratename, tracker)

    # Find the highest numbered output directory and copy its contents
    # to the final output directory.
    highest_out = find_highest_numbered_dir(resultsdir)
    if highest_out is not None:
        shutil.copytree(
            highest_out,
            resultsdir / "final",
        )

    tracker.mark_translation_finished()
    print("Translation finished.")
    print("Collecting static code quality measurements...")
    baseline_metrics = static_measurements_rust.static_rust_metrics(resultsdir / "vanilla_c2rust")
    xj_start_metrics = static_measurements_rust.static_rust_metrics(resultsdir / "00_out")
    xj_final_metrics = static_measurements_rust.static_rust_metrics(resultsdir / "final")

    print("Baseline from upstream c2rust:")
    pprint.pprint(baseline_metrics)

    print("Tenjin's initial, un-improved Rust output:")
    pprint.pprint(xj_start_metrics)

    print("Tenjin's final, improved Rust output:")
    pprint.pprint(xj_final_metrics)

    mb_mut_res = tracker.mb_mut_translation_results()
    if mb_mut_res:
        mb_mut_res.c2rust_baseline = baseline_metrics
        mb_mut_res.tenjin_initial = xj_start_metrics
        mb_mut_res.tenjin_final = xj_final_metrics

    record = tracker.finalize()
    if record is not None:
        with (resultsdir / "translation_metadata.json").open("w") as f:
            f.write(record.to_json(indent=2))

        results_snapshot = create_translation_snapshot(root, codebase, resultsdir, record)

        with (resultsdir / "translation_snapshot.json").open("w") as f:
            f.write(results_snapshot.to_json(indent=2))


def load_and_parse_guidance(guidance_path_or_literal):
    try:
        if guidance_path_or_literal == "":
            guidance = {}
        else:
            guidance = json.loads(guidance_path_or_literal)
    except json.JSONDecodeError:
        guidance = json.load(Path(guidance_path_or_literal).open("r", encoding="utf-8"))
    return guidance


def find_highest_numbered_dir(base: Path) -> Path | None:
    """
    Find the directory with the largest underscore-suffixed numeric prefix.
    """
    pattern = re.compile(r"^(\d+)_.*$")
    base = Path(base)

    if not base.exists():
        return None

    max_num = -1
    latest_dir = None

    for item in base.iterdir():
        if item.is_dir():
            match = pattern.match(item.name)
            if match:
                num = int(match.group(1))
                if num > max_num:
                    max_num = num
                    latest_dir = item

    return latest_dir if latest_dir else None


def get_multitool_toolchain(root: Path) -> str:
    return hermetic.get_toolchain_for_directory(root / "xj-improve-multitool")


def run_improve_multitool(root: Path, tool: str, args: list[str], dir: Path) -> CompletedProcess:
    # External Cargo tools need not be installed to be used, they only need to be on the PATH.
    # Since rustc plugins like xj-improve-multitool are tied to a specific toolchain,
    # it's not ideal to install them globally.
    # We could unconditionally `cargo install` into the _local/bin directory,
    # but it's a bit faster to just build & run from `target`.
    return hermetic.run_cargo_in(
        ["xj-improve-multitool", "--tool", tool, *args],
        cwd=dir,
        env_ext={
            "PATH": os.pathsep.join([
                str(root / "xj-improve-multitool" / "target" / "debug"),
                os.environ["PATH"],
            ]),
        },
        check=True,
    )


def run_un_unsafe_improvement(root: Path, dir: Path):
    """Iteratively remove unsafe blocks.

    We run xj-improve-multitool to extract a condensed (approximate) call graph,
    serialize it to JSON (one per local crate), then process the graph in
    topological order. For each function (well, SCC of functions), we'll remove
    the `unsafe` marker from the function signature, re-run cargo check,
    and roll back the edit if the check fails.

    There are two potentially significant optimizations one can do
    to reduce the number of times we run cargo check:
    1. process_function_sccs() could operate on multiple independent
       SCCs simultaneously.
    2. When function F mentions G, and G is known to be unsafe,
       we can assume F will also be judged unsafe by rustc.
       This is merely an approximation, since F may merely
       pass around G by address, for example, but in the common case
       it will be correct, and it's probably worth doing
       to unlock better scaling for larger crates. We are already tracking
       `unsafe_status` but are not yet using it.
    """

    def hacky_rewrite_fn_range(path: Path, lo: int, hi: int, replacement: str):
        """Rewrite a range in a file to a replacement string."""
        with path.open("r+") as f:
            content = f.read()
            assert (hi - lo) == len(replacement)
            content = content[:lo] + replacement + content[hi:]
            f.seek(0)
            f.write(content)
            f.truncate()

    class FunctionUnsafeNecessity(enum.Enum):
        """Status of a function with respect to unsafe blocks."""

        SAFE = 0
        UNSAFE = 1
        UNKNOWN = 2

    def default_function_unsafe_status(has_unsafe_span: bool) -> FunctionUnsafeNecessity:
        """This reflect our understanding of the function's safety requirements.
        c2rust must mark all possibly-unsafe functions as `unsafe`, because
        (A) neither rustc nor clippy can/will add unsafety markers on demand, and
        (B) rustc cannot remove unsafety markers on demand"""
        if has_unsafe_span:
            return FunctionUnsafeNecessity.UNKNOWN
        return FunctionUnsafeNecessity.SAFE

    def process_function_scc(
        cacg: CondensedSpanGraph,
        node: int,
        unsafe_spans: list[ExplicitSpan | None],
        unsafe_status: list[FunctionUnsafeNecessity],
    ):
        fnids = cacg.nodes[node]
        unknown_status_spans = []
        for fnid in fnids:
            if unsafe_status[fnid] == FunctionUnsafeNecessity.UNKNOWN:
                unknown_status_spans.append(unsafe_spans[fnid])

        if not unknown_status_spans:
            return

        rewriter = SpeculativeSpansEraser(
            unknown_status_spans,
            lambda span: Path(dir / cacg.files[span.fileid]),
        )
        rewriter.erase_spans()
        cp = hermetic.run_cargo_in(
            ["check", "--message-format=json"],
            cwd=dir,
            check=False,
            capture_output=True,
        )
        if cp.returncode != 0:
            # print("WARNING: cargo check failed -- rewritten SCC must have been unsafe after all...")
            # print(cp.stdout.decode("utf-8").splitlines())
            # print()
            # print()
            # print(cp.stderr)
            # print()
            rewriter.restore()
            for fnid in fnids:
                unsafe_status[fnid] = FunctionUnsafeNecessity.UNSAFE
        else:
            # If the check passes, we can safely mark all functions in this SCC as SAFE.
            for fnid in fnids:
                unsafe_status[fnid] = FunctionUnsafeNecessity.SAFE

            # rustc may have emitted warning about unnecessary unsafe blocks.
            # Neither `cargo fix` nor `cargo clippy --fix` will remove them,
            # so we do it manually.
            lines = cp.stdout.decode("utf-8").splitlines()
            for line in lines:
                j = json.loads(line)
                try:
                    if (
                        j["reason"] == "compiler-message"
                        and j["message"]["code"]["code"] == "unused_unsafe"
                    ):
                        tgtpath = dir / j["message"]["spans"][0]["file_name"]
                        lo = j["message"]["spans"][0]["byte_start"]
                        hi = j["message"]["spans"][0]["byte_end"]
                        hacky_rewrite_fn_range(tgtpath, lo, hi, " " * len("unsafe"))
                except:  # noqa: E722
                    print("TENJIN WARNING: unexpected JSON message from cargo check:", j)
                    continue

    def process_function_sccs(
        cacg: CondensedSpanGraph,
        ready_nodes: Sequence[int],
        unsafe_spans: list[ExplicitSpan | None],
        unsafe_status: list[FunctionUnsafeNecessity],
    ):
        # Each ready node is a strongly connected component (SCC)
        # in the call graph, which means that it is a set of functions
        # which are mutually recursive and therefore must all have
        # the same unsafe status.

        # We could go faster by modifying all the ready nodes simultaneously.
        # That would require some more sophistication on our part
        # to map unexpected-unsafe-operation to the corresponding
        # function signature.
        for node in ready_nodes:
            process_function_scc(cacg, node, unsafe_spans, unsafe_status)

    def find_unsafe_span_for_fn_sig(
        fnspan: ExplicitSpan,
    ) -> ExplicitSpan | None:
        filepath = Path(dir / cacg.files[fnspan.fileid])
        assert filepath.is_file(), "Expected a file at the path: " + str(filepath)
        contents = filepath.read_text("utf-8")
        snippet = contents[fnspan.lo : fnspan.hi]
        assert fnspan.hi > fnspan.lo, "Expected a non-empty span: " + str(fnspan)
        assert len(snippet) > 0, "Expected a non-empty snippet: " + snippet

        if snippet.startswith("fn") or snippet.startswith("\nfn"):
            # Can't be unsafe!
            return None

        fnidx = snippet.find(" fn ")
        if fnidx == -1:
            print(
                "WARNING: no fn signature found in span",
                fnspan,
                "in file",
                cacg.files[fnspan.fileid],
                "with snippet:",
                snippet,
                "\n\t\t\tlen snippet:",
                len(snippet),
            )
            return None
        snippet = contents[fnspan.lo : fnspan.lo + fnidx + 1]
        unsafe_idx = snippet.find("unsafe ")
        if unsafe_idx == -1:
            return None
        return ExplicitSpan(
            fileid=fnspan.fileid,
            lo=fnspan.lo + unsafe_idx,
            hi=fnspan.lo + unsafe_idx + len("unsafe"),
        )

    def remove_unsafe_blocks(cacg: CondensedSpanGraph):
        # The `elts` field contains full spans for functions;
        # we want to construct a sidecar list of the spans for the
        # unsafe keyword for each function (if it exists).
        unsafe_spans = [find_unsafe_span_for_fn_sig(fnspan) for fnspan in cacg.elts]
        unsafe_status = [default_function_unsafe_status(span is not None) for span in unsafe_spans]

        # print("fn spans:", cacg.elts)
        # print("unsafe spans:", unsafe_spans)
        # print("unsafe status:", unsafe_status)

        # Use graphlib to collect ready nodes in topological order
        # from the *reverse* of the call graph -- we want to process
        # the leaves first, so we can remove unsafe blocks from them
        # before we get to the functions that call them.
        g: graphlib.TopologicalSorter = graphlib.TopologicalSorter()
        for caller, callee in cacg.edges:
            # Note that the add method here is (node, predecessor)
            # which is already the reverse of the "conventional"
            # graph edge direction of src -> tgt / pred -> node.
            g.add(caller, callee)

        g.prepare()
        while g.is_active():
            ready_nodes = g.get_ready()
            process_function_sccs(cacg, ready_nodes, unsafe_spans, unsafe_status)
            g.done(*ready_nodes)

    with tempfile.TemporaryDirectory() as cacgdir:
        # Extract the call graph to a temporary directory.
        run_improve_multitool(root, "ExtractCACG", ["--cacg-json-outdir", cacgdir], dir)

        for item in Path(cacgdir).iterdir():
            if item.is_file() and item.name.endswith(".json"):
                cacg_json = json.load(item.open("rb"))
                cacg = CondensedSpanGraph.from_dict(cacg_json)  # type:ignore
                # Remove unsafe blocks from the call graph.
                remove_unsafe_blocks(cacg)


def hacky_whiteout_first_occurrence_within_first_n_bytes(contents: str, needle: str, n: int) -> str:
    # Find the first occurrence of the needle within the first n bytes
    first_n_bytes = contents[:n]
    start_idx = first_n_bytes.find(needle)
    if start_idx == -1:
        return contents  # No occurrence found, return original contents
    return contents[:start_idx] + (" " * len(needle)) + contents[start_idx + len(needle) :]


def run_trim_allows(root: Path, dir: Path):
    base_cp = hermetic.run_cargo_in(
        ["check", "--message-format=json"], cwd=dir, check=True, capture_output=True
    )

    if base_cp.returncode != 0:
        print("TENJIN NOTE: skipping trim-allows as initial cargo check failed.")
        return

    things_to_trim = [
        "  dead_code,",
        "  mutable_transmutes,",
        "  unused_assignments,",
        "  unused_mut,",
        "  unused_mut",
        "#![feature(extern_types)]",
        "#![allow(dead_code)]",
        "#![allow(unused_mut)]",
        "#![allow(mutable_transmutes)]",
        "#![allow(unused_assignments)]",
    ]

    def rough_parse_inner_attributes_len(rs_file_content: str) -> int:
        """Estimate the length of the inner attribute prefix in a Rust file."""
        lines = rs_file_content.splitlines()
        prelude_len = 0
        for line in lines:
            if line.startswith("#![") or line.startswith("    ") or line.startswith(")]"):
                prelude_len += len(line) + 1
            else:
                break
        return prelude_len

    for path in Path(dir).rglob("*.rs"):
        if not path.is_file():
            continue

        rewriter = SpeculativeFileRewriter(path)
        prelude_len = rough_parse_inner_attributes_len(rewriter.original_content)

        for thing in things_to_trim:

            def whiteout_first_thing(contents: str) -> str:
                return hacky_whiteout_first_occurrence_within_first_n_bytes(
                    contents, thing, prelude_len
                )

            rewriter.update_content_via(whiteout_first_thing)
            changes_made = rewriter.write()
            if changes_made:
                cp = hermetic.run_cargo_in(
                    ["check", "--message-format=json"], cwd=dir, check=False, capture_output=True
                )
                if cp.returncode != 0 or len(cp.stdout) > len(base_cp.stdout):
                    # If the check fails, we restore the original content.
                    print(
                        "TENJIN WARNING: cargo check failed after trimming",
                        thing,
                        "in",
                        dir,
                        "restoring original content.",
                    )
                    rewriter.restore()


def elapsed_ms_of_ns(start_ns: int, end_ns: int) -> float:
    """Calculate elapsed time in milliseconds from nanoseconds."""
    return (end_ns - start_ns) / 1_000_000.0


def run_improvement_passes(
    root: Path, output: Path, resultsdir: Path, cratename: str, tracker: ingest_tracking.TimingRepo
):
    def run_cargo_fmt(_root: Path, dir: Path) -> CompletedProcess:
        return hermetic.run_cargo_in(
            ["fmt"],
            cwd=dir,
            check=True,
            capture_output=True,
        )

    def run_cargo_fix(_root: Path, dir: Path) -> CompletedProcess:
        return hermetic.run_cargo_in(
            ["fix", "--allow-no-vcs", "--allow-dirty"],
            cwd=dir,
            check=True,
            capture_output=True,
        )

    improvement_passes: list[tuple[str, Callable[[Path, Path], CompletedProcess | None]]] = [
        ("fmt", run_cargo_fmt),
        ("fix", run_cargo_fix),
        (
            "trimdead",
            lambda root, dir: run_improve_multitool(
                root, "TrimDeadItems", ["--modify-in-place"], dir
            ),
        ),
        ("ununsafe", run_un_unsafe_improvement),
        # We run `fix` immedately after removing unsafe markers, because
        # removing an unsafe marker on a block may have rendered
        # the block safe, and therefore subject to removal.
        # But if we format first, the block may not be removable by `fix`!
        ("fix", run_cargo_fix),
        ("trim-allows", run_trim_allows),
        ("fmt", run_cargo_fmt),
    ]

    prev = output
    for counter, (tag, func) in enumerate(improvement_passes, start=1):
        newdir = resultsdir / f"{counter:02d}_{tag}"
        with tracker.tracking(f"improvement_pass_{counter:02d}_{tag}", newdir) as _step:
            start_ns = time.perf_counter_ns()
            shutil.copytree(prev, newdir)
            # Run the actual improvement pass, modifying the contents of `newdir`.
            cp_or_None: CompletedProcess | None = func(root, newdir)
            if cp_or_None is not None:
                _step.update_sub(cp_or_None)
            mid_ns = time.perf_counter_ns()
            # Use explicit toolchain for checks because c2rust may use extern_types which is unstable.
            hermetic.run_cargo_in(["check"], cwd=newdir, check=True)
            # Clean up the target directory so the next pass starts fresh.
            hermetic.run_cargo_in(["clean", "-p", cratename], cwd=newdir, check=True)
            end_ns = time.perf_counter_ns()

            core_ms = round(elapsed_ms_of_ns(start_ns, mid_ns))
            cleanup_ms = round(elapsed_ms_of_ns(mid_ns, end_ns))

            print(
                "Improvement pass",
                counter,
                tag,
                "took",
                core_ms,
                "ms (then cleanup took another",
                cleanup_ms,
                "ms)",
            )
            print()
            print()
            print()
            prev = newdir


def extract_preprocessor_definitions_from_compile_commands(
    parsed_compile_commands: list[dict],
    codebase: Path,
) -> ingest.PerFilePreprocessorDefinitions:
    """Extract preprocessor definitions from `compile_commands.json`"""
    definitions = {}
    for command_info in parsed_compile_commands:
        command_str = command_info.get("command", "")
        # command_info["directory"] is build directory, which can be
        # located anywhere; it has no relation to the source file path.
        relative_path = Path(command_info.get("file", "")).relative_to(codebase)
        defs: list[ingest.PreprocessorDefinition] = []
        args = shlex.split(command_str)
        i = 0
        while i < len(args):
            arg = args[i]
            i += 1
            if arg == "-D" and i + 1 < len(args):
                # If we find a -D, the next argument is a definition.
                key, _, value = args[i + 1].partition("=")
                i += 1  # Skip the value
                defs.append((key, value))
            elif arg.startswith("-D") and "=" in arg:
                # Handle -Dkey=value style definitions.
                key, _, value = arg[2:].partition("=")
                defs.append((key, value))
            if arg.startswith("-D"):
                # Handle -Dkey style definitions.
                defs.append((arg[2:], None))  # Add the definition without the -D prefix
        if defs:
            definitions[relative_path.as_posix()] = defs
    return definitions
