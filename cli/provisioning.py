from pathlib import Path
import platform
import os
import tempfile
import tarfile
import shutil
import subprocess
from urllib.parse import urlparse
from typing import Protocol
import json
import enum
import sys
import textwrap

from packaging.version import Version
import click

import repo_root
import hermetic
from sha256sum import compute_sha256
from constants import WANT, SYSROOT_NAME


class InstallationState(enum.Enum):
    NOT_INSTALLED = 0
    VERSION_OK = 1
    VERSION_MISMATCH = 2


class TrackingWhatWeHave:
    def __init__(self):
        self.localdir = repo_root.localdir()

        # This must be set in any code path that may call back to hermetic.run(), etc.
        # provision_desires() sets this, and individual calls to want_*() must do so as well.
        self.provisioning_depth = 0
        try:
            with open(Path(self.localdir, "config.10j-HAVE.json"), "r", encoding="utf-8") as f:
                self._have = json.load(f)
        except OSError:
            self._have = {}

    def save(self):
        with open(Path(self.localdir, "config.10j-HAVE.json"), "w", encoding="utf-8") as f:
            json.dump(self._have, f, indent=2, sort_keys=True)

    def note_we_have(self, name: str, version: Version | None = None, specifier: str | None = None):
        match [version is None, specifier is None]:
            case [True, True]:
                raise ValueError(f"For '{name}' must provide either version or specifier")
            case [False, False]:
                raise ValueError(f"For '{name}' must provide either version or specifier, not both")
            case _:
                pass

        had = self._have.get(name)
        now = str(version) if version else specifier
        self._have[name] = now
        if had != now:
            self.save()

    def note_removed(self, name: str):
        if name in self._have:
            del self._have[name]
        self.save()

    def query(self, name: str) -> str | None:
        return self._have.get(name)

    def compatible(self, name: str) -> InstallationState:
        assert name in WANT
        wanted_spec: str = WANT[name]

        if name not in self._have:
            return InstallationState.NOT_INSTALLED

        if self._have[name] == wanted_spec:
            return InstallationState.VERSION_OK

        return InstallationState.VERSION_MISMATCH


HAVE = TrackingWhatWeHave()


class ProvisioningError(Exception):
    pass


def sez(msg: str, ctx: str, err=False):
    click.echo("TENJIN SEZ: " + ctx + msg, err=err)


def download(url: str, filename: Path, first_attempt=True) -> None:
    # This import is relatively expensive (20 ms) and is rarely needed,
    # so it is imported here to avoid slowing down the common case.
    from urllib.request import urlretrieve  # noqa: PLC0415
    from urllib.error import HTTPError  # noqa: PLC0415
    from http.client import RemoteDisconnected  # noqa: PLC0415

    def report_potentially_transient_problem_and_retry():
        sez(
            "Hopefully this is a temporary issue and will resolve itself.",
            ctx="(download) ",
            err=True,
        )
        sez("I will wait for a few seconds then re-try, once.", ctx="(download) ", err=True)
        import time  # noqa: PLC0415

        time.sleep(6)
        download(url, filename, first_attempt=False)

    def report_insurmountable_error_and_die(e: Exception):
        sez(f"Failed to download {url}: {e}", ctx="(download) ", err=True)
        sez(
            "You might try deleting the `_local` directory and re-provisioning?",
            ctx="(download) ",
            err=True,
        )
        sys.exit(1)

    try:
        urlretrieve(url, filename)
    except HTTPError as e:
        if first_attempt and e.code in (500, 502, 503, 504):
            # These are HTTP error codes for (at least potentially) transient issues.
            sez(f"Server returned error {e} when downloading {url}", ctx="(download) ", err=True)
            report_potentially_transient_problem_and_retry()
            return

        report_insurmountable_error_and_die(e)
    except RemoteDisconnected as e:
        if first_attempt:
            sez(
                f"Server closed connection unexpectedly when downloading {url}",
                ctx="(download) ",
                err=True,
            )
            report_potentially_transient_problem_and_retry()
            return

        report_insurmountable_error_and_die(e)


# platform.system() in ["Linux", "Darwin"]


# See https://stackoverflow.com/questions/45125516/possible-values-for-uname-m
# See https://gist.github.com/skyzyx/d82b7d9ba05523dd1a9301fd282b32c4
def machine_normalized(aarch64="aarch64") -> str:
    x: dict[str, str] = {}
    for src in "arm64 arm64v8 arm64v9 armv8b armv8l aarch64_be aarch64".split():
        x[src] = aarch64

    for src in "amd64 AMD64 x64 x86_64".split():
        x[src] = "x86_64"

    m = platform.machine()
    return x.get(m, m)


def provision_desires(wanted: str):
    """This is the main entry point for provisioning.

    It is designed to be very fast when nothing needs updating:
    `provision_desires("all")` takes less than 1 ms.
    """
    if wanted == "uv":
        return
    assert wanted in "all llvm ocaml rust".split()
    assert HAVE.provisioning_depth == 0, "Re-provisioning loop detected!"
    HAVE.provisioning_depth += 1

    # Unlike the other stuff, we won't provision rustup ourselves,
    # so if it's not available, we should inform the user ASAP.
    if wanted in ("all", "rust"):
        require_rustup()

    # First time install?
    if wanted == "all" and HAVE.query("10j-reference-c2rust-tag") is None:

        def say(msg: str):
            sez(msg, ctx="(overall-provisioning) ")

        say(f"Provisioning local directory {HAVE.localdir}...")
        say("This involves downloading and extracting a few hundred megs of tarballs:")
        say("    Clang+LLVM, a sysroot, and misc build tools like CMake.")
        say("We'll also install Rust and OCaml, which will take a few minutes...")

    # We get these unconditionally, because both Rust and OCaml (and/or the
    # projects in those languages) end up needing them.
    want_10j_deps()
    want_10j_llvm()
    want_cmake()

    if wanted in ("all", "rust"):
        want_10j_rust_toolchains()

    if wanted in ("all", "ocaml"):
        want_dune()

    if wanted == "all":
        want_10j_reference_c2rust_tag()
        want_hayroll_maki()
        want_tree_sitter()
        want_uw_harvest_tree_sitter_c_preproc()

    HAVE.provisioning_depth -= 1


def require_rustup():
    def say(msg: str, fg: str = ""):
        if fg:
            msg = click.style(msg, fg=fg)
        sez(msg, ctx="(rust) ")

    rustup_installer = "rustup-installer.sh"

    # We don't run the installer ourselves because rustup pretty much requires
    # PATH modifications, and it's not our place to do that. In theory we could
    # have a hermetic copy of rustup + cargo etc but it seems silly because (A)
    # rustup is already hermetic enough, and (B) if someone is using Tenjin to
    # translate C to Rust, why on earth would they avoid having Rust installed?
    # Also one of TRACTOR's requirements is that translated Rust code works with
    # stable Rust, so pinning to a specific version of Rust would only result in
    # us not learning about bugs we really need to fix.
    #
    # HOWEVER: note that on a machine where only Tenjin's C compiler is available,
    # any cargo command that leads to compilation (cargo build, for many projects,
    # and also things like `rustup +nightly component add miri`, always) must be
    # run via 10j.
    def complain_about_tool_then_die(tool: str):
        say(f"{tool} is not installed, or is not available on your $PATH", fg="red")
        match platform.system():
            case "Linux":
                say("Please install Rust using rustup (or via your package manager).")
            case "Darwin":
                say("Please install Rust using rustup (or via Homebrew).")
            case sysname:
                say(f"Tenjin doesn't yet support {sysname}, sorry!", fg="red")
                sys.exit(1)

        download("https://sh.rustup.rs", Path(rustup_installer))
        subprocess.check_call(["chmod", "+x", rustup_installer])

        say("")
        say("For your convenience, I've downloaded the rustup installer script,")
        say("so you can just run")
        say(click.style(f"                  ./{rustup_installer}", bold=True))
        say("")
        say("It will interactively prompt you for the details of how and where")
        say("to install Rust. Most people choose the default options.")
        say("")
        say("Once you can run `cargo --version`, please re-run provisioning", fg="green")
        sys.exit(1)

    if shutil.which("rustc") is None:
        complain_about_tool_then_die("Rust")
    if shutil.which("cargo") is None:
        complain_about_tool_then_die("cargo")
    if shutil.which("rustup") is None:
        complain_about_tool_then_die("rustup")

    # At this point, the installer's job is done.
    if Path(rustup_installer).is_file():
        os.remove(rustup_installer)


class Provisioner(Protocol):
    def __call__(self, version: str, keyname: str) -> None:
        """Provision the given version of the software into the given localdir."""


def want(
    keyname: str,
    lowername: str,
    titlename: str,
    provisioner: Provisioner,
):
    match HAVE.compatible(keyname):
        case InstallationState.VERSION_OK:
            return
        case InstallationState.VERSION_MISMATCH:
            sez(f"{titlename} version is outdated; re-provisioning...", ctx=f"({lowername}) ")
            provisioner(version=WANT[keyname], keyname=keyname)
        case InstallationState.NOT_INSTALLED:
            provisioner(version=WANT[keyname], keyname=keyname)


def want_cmake() -> None:
    want("10j-cmake", "cmake", "CMake", provision_cmake_with)


def want_dune():
    want("10j-dune", "dune", "Dune", provision_dune_with)


def want_opam():
    want("10j-opam", "opam", "opam", provision_opam_with)


def want_ocaml():
    want("10j-ocaml", "ocaml", "OCaml", provision_ocaml_with)


def want_10j_llvm():
    want("10j-llvm", "llvm", "LLVM", provision_10j_llvm_with)
    want_10j_sysroot_extras()


def want_10j_rust_toolchains():
    """This must not lead back to hermetic.common_helper_for_run()."""
    want("10j-xj-improve-multitool-toolchain", "rust", "Rust", provision_10j_rust_toolchain_with)
    want("10j-xj-default-rust-toolchain", "rust", "Rust", provision_10j_rust_toolchain_with)


def want_10j_reference_c2rust_tag():
    def provision_10j_reference_c2rust_tag_with(
        version: str,
        keyname: str,
    ):
        def say(msg: str):
            sez(msg, ctx="(c2rust) ")

        localdir = HAVE.localdir

        xj_upstream_c2rust = hermetic.xj_upstream_c2rust(localdir)
        if xj_upstream_c2rust.is_dir():
            say(f"Fetching and resetting C2Rust to version {version} ...")
            subprocess.check_call(["git", "fetch", "--all"], cwd=str(xj_upstream_c2rust))
            subprocess.check_call(
                ["git", "switch", "--detach", version], cwd=str(xj_upstream_c2rust)
            )
        else:
            say(f"Cloning C2Rust {version} ...")

            stdout_path = Path(localdir, "xj-c2rust-clone.log")
            stderr_path = Path(localdir, "xj-c2rust-clone.err")
            hermetic.run_command_with_progress(
                [
                    "git",
                    "clone",
                    "--branch",
                    version,
                    "https://github.com/immunant/c2rust.git",
                    str(xj_upstream_c2rust),
                ],
                stdout_file=stdout_path,
                stderr_file=stderr_path,
            )

        rebuild_10j_upstream_c2rust(xj_upstream_c2rust)
        HAVE.note_we_have(keyname, specifier=version)

    want(
        "10j-reference-c2rust-tag",
        "c2rust",
        "Upstream C2Rust",
        provision_10j_reference_c2rust_tag_with,
    )


def rebuild_10j_upstream_c2rust(xj_upstream_c2rust: Path):
    stdout_path = Path(xj_upstream_c2rust, "xj-c2rust-build.log")
    stderr_path = Path(xj_upstream_c2rust, "xj-c2rust-build.err")

    sez("Building upstream C2Rust...", ctx="(c2rust) ")
    hermetic.run_command_with_progress(
        [
            "cargo",
            hermetic.tenjin_cargo_toolchain_specifier(),
            "build",
            "--locked",
            "-p",
            "c2rust",
            "-p",
            "c2rust-transpile",
        ],
        stdout_file=stdout_path,
        stderr_file=stderr_path,
        cwd=xj_upstream_c2rust,
        env_ext=hermetic.cargo_encoded_rustflags_env_ext(),
    )
    # Ensure a clean checkout for future updates
    stdout_path.unlink(missing_ok=True)
    stderr_path.unlink(missing_ok=True)


def want_hayroll_maki():
    def provision_hayroll_maki_with(
        version: str,
        keyname: str,
    ):
        def say(msg: str):
            sez(msg, ctx="(hayroll-maki) ")

        localdir = HAVE.localdir

        xj_hayroll_maki = hermetic.xj_hayroll_maki(localdir)
        if xj_hayroll_maki.is_dir():
            say(f"Fetching and resetting @UW-HARVEST/Maki to commit {version} ...")
            subprocess.check_call(["git", "fetch", "--all"], cwd=str(xj_hayroll_maki))
            subprocess.check_call(
                ["git", "checkout", "--detach", version], cwd=str(xj_hayroll_maki)
            )
        else:
            say(f"Cloning @UW-HARVEST/Maki {version} ...")

            stdout_path = Path(localdir, "xj-hayroll-maki-clone.log")
            stderr_path = Path(localdir, "xj-hayroll-maki-clone.err")
            hermetic.run_command_with_progress(
                [
                    "git",
                    "clone",
                    "https://github.com/Aarno-Labs/Maki.git",
                    str(xj_hayroll_maki),
                ],
                stdout_file=stdout_path,
                stderr_file=stderr_path,
            )
            subprocess.check_call(
                ["git", "checkout", "--detach", version], cwd=str(xj_hayroll_maki)
            )

        rebuild_10j_hayroll_maki(xj_hayroll_maki)
        HAVE.note_we_have(keyname, specifier=version)

    want(
        "uw-harvest-maki",
        "uw-harvest-maki",
        "Hayroll's fork of Maki",
        provision_hayroll_maki_with,
    )


def rebuild_10j_hayroll_maki(xj_hayroll_maki: Path):
    config_stdout_path = Path(xj_hayroll_maki, "xj-hayroll-maki-config.log")
    config_stderr_path = Path(xj_hayroll_maki, "xj-hayroll-maki-config.err")
    build_stdout_path = Path(xj_hayroll_maki, "xj-hayroll-maki-build.log")
    build_stderr_path = Path(xj_hayroll_maki, "xj-hayroll-maki-build.err")

    cmd = [
        "cmake",
        "-B",
        str(xj_hayroll_maki / "build"),
        "-S",
        xj_hayroll_maki,
    ]

    # We need to tell CMake where our hermetic copy of zlib is, on Linux;
    # on Mac, we get zlib via homebrew, so it gets picked up automatically.
    if (hermetic.xj_build_deps(HAVE.localdir) / "usr" / "lib" / "libz.so").is_file():
        cmd.append("-DZLIB_ROOT={}".format(hermetic.xj_build_deps(HAVE.localdir)))

    sez("Configuring Hayroll's fork of Maki...", ctx="(hayroll-maki) ")
    hermetic.run_command_with_progress(
        cmd,
        stdout_file=config_stdout_path,
        stderr_file=config_stderr_path,
    )
    sez("Building Hayroll's fork of Maki...", ctx="(hayroll-maki) ")
    hermetic.run_command_with_progress(
        ["cmake", "--build", str(xj_hayroll_maki / "build"), "--", f"-j{os.cpu_count()}"],
        stdout_file=build_stdout_path,
        stderr_file=build_stderr_path,
        cwd=xj_hayroll_maki,
    )
    # Ensure a clean checkout for future updates
    config_stdout_path.unlink(missing_ok=True)
    config_stderr_path.unlink(missing_ok=True)
    build_stdout_path.unlink(missing_ok=True)
    build_stderr_path.unlink(missing_ok=True)


def want_uw_harvest_tree_sitter_c_preproc():
    def rebuild_10j_uw_harvest_tree_sitter_c_preproc(git_root: Path):
        build_stdout_path = Path(git_root, "xj-uw-harvest-tree-sitter-c_preproc-build.log")
        build_stderr_path = Path(git_root, "xj-uw-harvest-tree-sitter-c_preproc-build.err")

        # Note that Hayroll's build system assumes we're doing an in-tree build
        # with Make, not CMake.

        sez("Building UW-HARVEST/tree-sitter-c_preproc...", ctx="(hayroll) ")
        hermetic.run_command_with_progress(
            ["make", f"-j{os.cpu_count()}"],
            stdout_file=build_stdout_path,
            stderr_file=build_stderr_path,
            cwd=git_root,
        )
        # Ensure a clean checkout for future updates
        build_stdout_path.unlink(missing_ok=True)
        build_stderr_path.unlink(missing_ok=True)

    def provision_uw_harvest_tree_sitter_c_preproc_with(
        version: str,
        keyname: str,
    ):
        def say(msg: str):
            sez(msg, ctx="(uw-harvest-tree-sitter-c_preproc) ")

        localdir = HAVE.localdir

        target_dir = localdir / "tree-sitter-c_preproc"
        if target_dir.is_dir():
            say(f"Fetching and resetting @UW-HARVEST/tree-sitter_c_preproc to commit {version} ...")
            subprocess.check_call(["git", "fetch", "--all"], cwd=str(target_dir))
            subprocess.check_call(["git", "checkout", "--detach", version], cwd=str(target_dir))
        else:
            say(f"Cloning @UW-HARVEST/tree-sitter_c_preproc {version} ...")

            stdout_path = Path(localdir, "xj-uw-harvest-tree-sitter-c_preproc-clone.log")
            stderr_path = Path(localdir, "xj-uw-harvest-tree-sitter-c_preproc-clone.err")
            hermetic.run_command_with_progress(
                [
                    "git",
                    "clone",
                    "https://github.com/UW-HARVEST/tree-sitter-c_preproc.git",
                    str(target_dir),
                ],
                stdout_file=stdout_path,
                stderr_file=stderr_path,
            )
            subprocess.check_call(["git", "checkout", "--detach", version], cwd=str(target_dir))

        rebuild_10j_uw_harvest_tree_sitter_c_preproc(target_dir)
        HAVE.note_we_have(keyname, specifier=version)

    want(
        "uw-harvest-tree-sitter-c_preproc",
        "uw-harvest-tree-sitter-c_preproc",
        "UW-HARVEST/tree-sitter-c_preproc",
        provision_uw_harvest_tree_sitter_c_preproc_with,
    )


def want_tree_sitter():
    def rebuild_10j_tree_sitter(git_root: Path):
        build_stdout_path = Path(git_root, "xj-tree-sitter-build.log")
        build_stderr_path = Path(git_root, "xj-tree-sitter-build.err")

        sez("Building tree-sitter...", ctx="(hayroll) ")
        hermetic.run_command_with_progress(
            ["make", f"-j{os.cpu_count()}"],
            stdout_file=build_stdout_path,
            stderr_file=build_stderr_path,
            cwd=git_root,
        )
        # Ensure a clean checkout for future updates
        build_stdout_path.unlink(missing_ok=True)
        build_stderr_path.unlink(missing_ok=True)

    def provision_tree_sitter_with(
        version: str,
        keyname: str,
    ):
        def say(msg: str):
            sez(msg, ctx="(hayroll) ")

        localdir = HAVE.localdir

        target_dir = localdir / "tree-sitter"
        if target_dir.is_dir():
            say(f"Fetching and resetting tree-sitter to commit {version} ...")
            subprocess.check_call(["git", "fetch", "--all"], cwd=str(target_dir))
            subprocess.check_call(["git", "checkout", "--detach", version], cwd=str(target_dir))
        else:
            say(f"Cloning tree-sitter {version} ...")

            stdout_path = Path(localdir, "xj-tree-sitter-clone.log")
            stderr_path = Path(localdir, "xj-tree-sitter-clone.err")
            hermetic.run_command_with_progress(
                [
                    "git",
                    "clone",
                    "https://github.com/tree-sitter/tree-sitter.git",
                    str(target_dir),
                ],
                stdout_file=stdout_path,
                stderr_file=stderr_path,
            )
            subprocess.check_call(["git", "checkout", "--detach", version], cwd=str(target_dir))

        rebuild_10j_tree_sitter(target_dir)
        HAVE.note_we_have(keyname, specifier=version)

    want(
        "tree-sitter",
        "tree-sitter",
        "tree-sitter",
        provision_tree_sitter_with,
    )


def want_10j_sysroot_extras():
    if platform.system() != "Linux":
        return

    def provision_10j_sysroot_extras_with(
        version: str,
        keyname: str,
    ):
        filename = f"xj-bullseye-sysroot-extras_{machine_normalized()}.tar.xz"
        url = f"https://github.com/Aarno-Labs/tenjin-build-deps/releases/download/{version}/{filename}"
        localdir = HAVE.localdir

        tarball = hermetic.xj_llvm_root(localdir) / filename
        download(url, tarball)

        tmp_dest = hermetic.xj_llvm_root(localdir) / "tmp"
        tmp_dest.mkdir()

        shutil.unpack_archive(tarball, tmp_dest, filter="tar")
        tarball.unlink()

        # We use non-normalized platform.machine() here because we want to match
        # the (non-normalized) Linux native naming convention.
        triple = f"{platform.machine()}-linux-gnu"
        shutil.copytree(
            tmp_dest / "debian-bullseye_gcc_glibc" / machine_normalized() / "usr_lib",
            hermetic.xj_llvm_root(localdir) / "sysroot" / "usr" / "lib" / triple,
            dirs_exist_ok=True,
        )

        # We need the .a files to enable static linking for our hermetic clang.
        shutil.copytree(
            tmp_dest / "debian-bullseye_gcc_glibc" / machine_normalized() / "usr_lib_gcc",
            hermetic.xj_llvm_root(localdir) / "sysroot" / "usr" / "lib" / "gcc" / triple / "10",
            dirs_exist_ok=True,
        )

        shutil.rmtree(tmp_dest)

        HAVE.note_we_have(keyname, specifier=version)

    want(
        "10j-bullseye-sysroot-extras",
        "sysroot-extras",
        "sysroot-extras",
        provision_10j_sysroot_extras_with,
    )


def want_10j_deps():
    if platform.system() == "Darwin":
        return

    want(
        "10j-build-deps",
        "10j-build-deps",
        "Tenjin build deps",
        provision_10j_deps_with,
    )


def provision_10j_rust_toolchain_with(version: str, keyname: str):
    """This must not lead back to hermetic.common_helper_for_run()."""

    # Examples of expected toolchain specs: "1.88.0" or "nightly-2025-03-03".
    # Specifying the point release helps avoid redundant downloads. Observe:
    #     $ docker run --rm -it rust:1.88-alpine rustc +1.88.0 --version
    #     rustc 1.88.0 (6b00bc388 2025-06-23)
    #     $ docker run --rm -it rust:1.88-alpine rustc +1.88 --version
    #     info: syncing channel updates for '1.88-x86_64-unknown-linux-musl'
    #     ...
    #     $ docker run --rm -it rust:1.88-alpine rustup toolchain list
    #     1.88.0-x86_64-unknown-linux-musl (active, default)
    toolchain_spec = version
    assert not toolchain_spec.startswith("+")
    assert not toolchain_spec == "stable"

    def say(msg: str):
        sez(msg, ctx="(rust) ")

    # The rustup.sh installer defaults to the 'default' profile, which provides clippy,
    # but the 'minimal' profile (as used in official Rust docker images) does not.
    # The easiest thing to do is unconditionally add the clippy component.
    # This will also download the requested toolchain if it is not already installed.
    cmd = ["rustup", "component", "add", "--toolchain", toolchain_spec, "clippy", "rustfmt"]
    if toolchain_spec.startswith("nightly"):
        cmd.append("rustc-dev")
        cmd.append("llvm-tools")

    say(f"Installing Rust toolchain {toolchain_spec}...")
    log_path = repo_root.localdir() / "rustup.log.txt"
    hermetic.run_command_with_progress(
        cmd, stdout_file=log_path, stderr_file=log_path.with_suffix(".err"), suppress_helper=True
    )

    HAVE.note_we_have(keyname, specifier=toolchain_spec)


def grab_opam_stdout(args: list[str]) -> str:
    cp = hermetic.run_opam(
        args,
        check=True,
        capture_output=True,
    )
    return cp.stdout.decode("utf-8").strip()


# Prerequisite: opam provisioned.
def grab_opam_version_str() -> str:
    return grab_opam_stdout(["--version"])


# Prerequisite: opam and ocaml provisioned.
def grab_ocaml_version_str() -> str:
    return grab_opam_stdout(["exec", "--", "ocamlc", "--version"])


# Prerequisite: opam and dune provisioned.
def grab_dune_version_str() -> str:
    return grab_opam_stdout(["exec", "--", "dune", "--version"])


def provision_ocaml_with(version: str, keyname: str):
    provision_ocaml(version)

    HAVE.note_we_have(keyname, version=Version(version))

    hermetic.run_opam(["config", "report"], check=False)

    if Version(grab_ocaml_version_str()) != Version(version):
        raise ProvisioningError(
            f"Expected OCaml version {version}, got {grab_ocaml_version_str()}."
        )


def provision_ocaml(ocaml_version: str):
    want_opam()

    def say(msg: str):
        sez(msg, ctx="(ocaml) ")

    TENJIN_SWITCH = "tenjin"

    def install_ocaml():
        if not hermetic.opam_non_hermetic():
            # For hermetic installations, we will simply bulldoze the existing
            # opam root and start fresh. For non-hermetic installations, we'll
            # try to reuse what's already there.
            opamroot = HAVE.localdir / "opamroot"
            if opamroot.is_dir():
                shutil.rmtree(opamroot)

        sandboxing_arg = infer_bwrap_sandboxing_args()

        cp = hermetic.run_opam(["config", "report"], eval_opam_env=False, capture_output=True)
        if b"please run `opam init'" in cp.stderr:
            say("================================================================")
            say("Initializing opam; this will take about half a minute...")
            say("      (subsequent output comes from `opam init --bare`)")
            say("----------------------------------------------------------------")
            say("")
            hermetic.check_call_opam(
                ["init", "--bare", "--no-setup", "--disable-completion", *sandboxing_arg],
                eval_opam_env=False,
            )

        cp = hermetic.run_opam(
            ["switch", "list"], eval_opam_env=False, check=True, capture_output=True
        )
        if TENJIN_SWITCH in cp.stdout.decode("utf-8"):
            if grab_ocaml_version_str() == ocaml_version:
                say("================================================================")
                say("Reusing cached OCaml, saving a few minutes of compiling...")
                say("----------------------------------------------------------------")
                return
            else:
                say("Removing cached OCaml switch due to version mismatch.")
                say(f"  had {grab_ocaml_version_str()}, want {ocaml_version}.")
                hermetic.check_call_opam(
                    ["switch", "remove", TENJIN_SWITCH, "-y"], eval_opam_env=False
                )

        say("")
        say("================================================================")
        say("Installing OCaml; this will take four-ish minutes to compile...")
        say("      (subsequent output comes from `opam switch create`)")
        say("----------------------------------------------------------------")

        hermetic.check_call_opam(
            ["switch", "create", TENJIN_SWITCH, ocaml_version, "--no-switch"],
            eval_opam_env=False,
            env_ext={
                "OPAMNOENVNOTICE": "1",
                "CC": str(hermetic.xj_llvm_root(HAVE.localdir) / "bin" / "clang"),
                "CXX": str(hermetic.xj_llvm_root(HAVE.localdir) / "bin" / "clang++"),
            },
        )

    install_ocaml()


def provision_debian_bullseye_sysroot_with(dest_sysroot: Path):
    def say(msg: str):
        sez(msg, ctx="(sysroot) ")

    say("Downloading and unpacking sysroot tarball, will take maybe 10 s...")

    CHROME_LINUX_SYSROOT_URL = "https://commondatastorage.googleapis.com/chrome-linux-sysroot"

    # These don't go in WANT because they're quite stable;
    # we don't expect to need a new version, ever.
    DEBIAN_BULLSEYE_SYSROOT_SHA256SUMS = {
        "x86_64": "36a164623d03f525e3dfb783a5e9b8a00e98e1ddd2b5cff4e449bd016dd27e50",
        "aarch64": "2f915d821eec27515c0c6d21b69898e23762908d8d7ccc1aa2a8f5f25e8b7e18",
        "armhf": "47b3a0b161ca011b2b33d4fc1ef6ef269b8208a0b7e4c900700c345acdfd1814",
    }
    tarball_sha256sum = DEBIAN_BULLSEYE_SYSROOT_SHA256SUMS[machine_normalized()]

    url = CHROME_LINUX_SYSROOT_URL + "/" + tarball_sha256sum

    if dest_sysroot.is_dir():
        shutil.rmtree(dest_sysroot)
    dest_sysroot.mkdir()
    tarball = dest_sysroot / "tenjin-sysroot.tar.xz"

    download(url, tarball)
    sha256sum = compute_sha256(tarball)
    if sha256sum != tarball_sha256sum:
        raise ProvisioningError("Sysroot hash verification failed!")
    shutil.unpack_archive(tarball, dest_sysroot, filter="tar")
    tarball.unlink()


def provision_opam_binary_with(opam_version: str) -> None:
    def say(msg: str):
        sez(msg, ctx="(opam) ")

    localdir = HAVE.localdir
    # If the system happens to have a copy of a suitable version of opam, grab it.
    sys_opam = shutil.which("opam")
    if sys_opam is not None:
        sys_opam_version = subprocess.check_output([sys_opam, "--version"]).decode("utf-8")
        if Version(sys_opam_version) >= Version(opam_version):
            say(f"Symlinking to a suitable version of opam at {sys_opam}")
            os.symlink(sys_opam, str(localdir / "opam"))
            return

    # Otherwise, we'll need to run the installer to get it.
    say("Downloading a local copy of opam...")
    installer_sh = localdir / "install-opam.sh"
    download("https://opam.ocaml.org/install.sh", installer_sh)
    if not installer_sh.is_file():
        raise ProvisioningError(f"Unable to download installer script for opam {opam_version}.")
    subprocess.check_call(["sh", installer_sh, "--download-only", "--version", opam_version])
    tagged = list(Path(".").glob(f"opam-{opam_version}-*"))
    assert len(tagged) == 1
    tagged_path = tagged[0]
    subprocess.check_call(["chmod", "+x", tagged_path])
    tagged_path.replace(localdir / "opam")

    if hermetic.running_in_ci():
        dotlocalbin = Path.home() / ".local" / "bin"
        if str(dotlocalbin) in os.environ["PATH"]:
            if not dotlocalbin.is_dir():
                dotlocalbin.mkdir(parents=True)

            # XREF:ci-opam-paths
            # We are in CI, but didn't have opam on the path already.
            # Install it where (A) nrsr.yaml will cache it and (B) it'll be on PATH.
            # Then our next CI run will be faster.
            shutil.copy(Path(localdir, "opam"), dotlocalbin / "opam")
        else:
            click.echo("WARNING: ~/.local/bin not on PATH anymore?!? OCaml cache won't work.")


def provision_dune_with(version: str, keyname: str):
    provision_dune(version)

    HAVE.note_we_have(keyname, version=Version(version))

    if Version(grab_dune_version_str()) != Version(version):
        raise ProvisioningError(f"Expected dune version {version}, got {grab_dune_version_str()}.")


def infer_bwrap_sandboxing_args() -> list[str]:
    if platform.system() != "Linux":
        return []  # bwrap is Linux-only

    bwrap_path = hermetic.xj_build_deps(HAVE.localdir) / "bin" / "bwrap"
    if not bwrap_path.is_file():
        return ["--disable-sandboxing"]  # no bwrap, no sandboxing

    # Bubblewrap does not work inside Docker containers, at least not without
    # heinous workarounds, if we're in Docker then we don't really need it anyway.
    # So we'll try running a trivial command with it; if it fails, we'll tell opam
    # not to use it.
    #
    # We really want to run a statically linked binary, since
    # for a dynamically linked binary we'd need to add symlinks
    # for whatever libraries it needs, which can be platform-dependent.
    # But which binaries are statically linked will vary by distro.
    # So we'll compile one ourselves.
    tru_c_src = "int main() { return 0; }"
    binary_path = Path.cwd() / "tru"

    with tempfile.NamedTemporaryFile(
        suffix=".c", mode="w", encoding="utf-8", delete=False
    ) as temp_c_file:
        temp_c_file.write(tru_c_src)
        temp_c_file_path = temp_c_file.name

    try:
        compile_cmd = ["clang", temp_c_file_path, "-static", "-o", str(binary_path)]
        hermetic.run_shell_cmd(compile_cmd, check=True)
        os.chmod(binary_path, 0o755)

        subprocess.check_call([bwrap_path, "--ro-bind", Path.cwd(), "/", "/tru"])
    except subprocess.CalledProcessError as e:
        print(f"TENJIN: Compilation of bubblewrap-able static binary failed with error: {e.stderr}")
    except Exception as e:
        print(f"An error occurred: {str(e)}")
    else:
        return []  # do not disable sandboxing
    finally:
        Path(temp_c_file_path).unlink(missing_ok=True)
        binary_path.unlink(missing_ok=True)

    # If we reach here, it means we didn't return on the happy path
    return ["--disable-sandboxing"]


# Precondition: not installed, or version too old.
def provision_dune(dune_version: str):
    want_ocaml()

    def say(msg: str):
        sez(msg, ctx="(opam) ")

    try:
        actual_version = Version(grab_dune_version_str())
        if actual_version == Version(dune_version):
            # We only get here when the HAVE cache is incorrect: it thinks dune
            # is not installed or is out of date, but dune is in fact installed
            # with the desired version.
            say(f"Dune {actual_version} is already installed.")
            return

        say(f"Found dune version {actual_version}, but we need {dune_version}.")
    except subprocess.CalledProcessError:
        # This is expected if we don't have dune installed yet.
        pass

    say("")
    say("================================================================")
    say("Installing Dune; this will take a minute to compile...")
    say("      (subsequent output comes from `opam install dune`)")
    say("----------------------------------------------------------------")

    try:
        # Try installing from opam registry first
        hermetic.check_call_opam(["install", f"dune.{dune_version}"])
    except subprocess.CalledProcessError:
        say("Failed to install dune from opam registry.")
        provision_dune_from_source(dune_version, say)


def provision_dune_from_source(dune_version: str, say):
    """
    Download and install dune from source when opam registry installation fails.
    """
    # GitHub releases URL pattern
    say("Installing dune from source...")
    dune_git_url = f"git+https://github.com/ocaml/dune.git#{dune_version}"
    hermetic.check_call_opam(["pin", "add", "--yes", f"dune.{dune_version}", dune_git_url])


def provision_opam_with(version: str, keyname: str):
    def say(msg: str):
        sez(msg, ctx="(opam) ")

    provision_opam_binary_with(version)
    HAVE.note_we_have(keyname, version=Version(version))

    opam_version_seen = grab_opam_version_str()
    say(f"opam version: {opam_version_seen}")
    # Opam provisioning is flexible; we'll treat greater versions as if they are
    # the requested version, in some cases (primarily to make CI faster).
    # So we don't want to fail if the version is greater than requested.


def provision_cmake_with(version: str, keyname: str):
    def fmt_url(tag: str) -> str:
        return f"https://github.com/Kitware/CMake/releases/download/v{version}/cmake-{version}-{tag}.tar.gz"

    def mk_url() -> str:
        match [platform.system(), machine_normalized()]:
            case ["Linux", "x86_64"]:
                return fmt_url("linux-x86_64")
            case ["Linux", "aarch64"]:
                return fmt_url("linux-aarch64")
            case ["Darwin", _]:
                return fmt_url("macos-universal")
            case sys_mach:
                raise ProvisioningError(
                    f"Tenjin does not yet support {sys_mach} for acquiring CMake."
                )

    cmake_dir = HAVE.localdir / "cmake"
    if cmake_dir.is_dir():
        # Clear prior installation to avoid tarball unpacking conflicts
        shutil.rmtree(cmake_dir)
    download_and_extract_tarball(mk_url(), cmake_dir, ctx="(cmake) ")

    if platform.system() == "Darwin" and (cmake_dir / "CMake.app").is_dir():
        # The tarball for macOS contains a .app bundle; we'll make a symlink
        # into it to create paths consistent with other platforms.
        cmake_app_bin = cmake_dir / "CMake.app" / "Contents" / "bin"
        os.symlink(cmake_app_bin, cmake_dir / "bin")

    HAVE.note_we_have(keyname, version=Version(version))
    # Running validation after we've set our HAVE version
    # avoids infinite loops.
    validate_actual_cmake_version(version)


def validate_actual_cmake_version(expected_version: str):
    out: bytes = hermetic.run_shell_cmd("cmake --version", check=True, capture_output=True).stdout
    outstr = out.decode("utf-8")
    lines = outstr.splitlines()
    if lines == []:
        raise ProvisioningError("CMake version command returned no output.")
    else:
        match lines[0].split():
            case ["cmake", "version", version]:
                if Version(version) != Version(expected_version):
                    raise ProvisioningError(
                        f"Expected CMake version {expected_version}, got {version}."
                    )
            case _:
                raise ProvisioningError(f"Unexpected output from CMake version command:\n{outstr}")


def provision_10j_llvm_with(version: str, keyname: str):
    localdir = HAVE.localdir

    assert "@" in version, "Expected version of the form 'LLVM_VERSION@tenjin-build-deps-release'"
    llvm_version, release = version.split("@", 1)

    def provision_clang_config_files(sysroot_path):
        match platform.system():
            case "Linux":
                platform_specific_stuff = textwrap.dedent(f"""\
                    # OCaml will explicitly pass -L/usr/lib/x86_64-linux-gnu
                    # which throws a wrench in things because the version of glibc in the
                    # sysroot assumes that the libc_nonshared.a file (implicitly linked by
                    # the libc.so linker script) defines __libc_csu_init, but that
                    # symbol has been removed in more recent versions of glibc. Thus,
                    # combining old Scrt1.o from within the sysroot, with new libc_nonshared.a
                    # from outside of the sysroot, results in a link error.
                    # To avoid this outcome, we forcibly add the within-sysroot path
                    # to take precedence over the external system path.
                    -L <CFGDIR>/../sysroot/usr/lib/{platform.machine()}-linux-gnu/

                    # This one's unfortunate. LLD defaults to --no-allow-shlib-undefined
                    # but the libgcc_s.so.1 shipped with Ubuntu 22.04 has an undefined
                    # symbol for _dl_find_object@GLIBC_2.35
                    -Wl,--allow-shlib-undefined
                    """)
            case _:
                platform_specific_stuff = ""

        # Write config files to make sure that the sysroot is used by default.
        for name in ("clang", "clang++", "cc", "c++"):
            with open(
                hermetic.xj_llvm_root(localdir) / "bin" / f"{name}.cfg", "w", encoding="utf-8"
            ) as f:
                f.write(
                    textwrap.dedent(f"""\
                        --sysroot {sysroot_path}
                        {platform_specific_stuff}
                        """)
                )

    def provision_debian_sysroot():
        provision_debian_bullseye_sysroot_with(hermetic.xj_llvm_root(localdir) / SYSROOT_NAME)

        #                   COMMENTARY(goblint-cil-gcc-wrapper)
        # Okay, this one is unfortunate. We generally only care about software that
        # builds with Clang. But CodeHawk depends on goblint-cil, which uses C code
        # in its config step that has GCC extensions which Clang doesn't support, &
        # thus goblint-cil looks specifically for a GCC binary. So what we're gonna
        # do here is write out a wrapper script for goblint-cil to find, which will
        # intercept the GCC-specific stuff in the code it compiles and patch it out
        # before passing it on to Clang. Hurk!
        sadness = hermetic.xj_llvm_root(localdir) / "goblint-sadness"
        sadness.mkdir(exist_ok=True)
        gcc_wrapper_path = sadness / "gcc"
        with open(gcc_wrapper_path, "w", encoding="utf-8") as f:
            f.write(
                textwrap.dedent("""\
                    #!/bin/sh

                    # See COMMENTARY(goblint-cil-gcc-wrapper) in cli/provisioning.py

                    if [ "$1" = "--version" ]; then
                        echo "gcc (GCC) 7.999.999"
                    elif [ "$*" = "-D_GNUCC machdep-ml.c -o machdep-ml.exe" ]; then

                        CFILE=machdep-ml-clangcompat.c
                        # Remove references to Clang-unsupported type _Float128.
                        cat machdep-ml.c \
                                | sed 's/_Float128 _Complex/struct { char _[32]; }/g' \
                                | sed 's/_Float128/struct { char _[16]; }/g' > "$CFILE" || {
                            rm -f "$CFILE"
                            exit 1
                        }

                        exec clang -D_GNUCC "$CFILE" -o machdep-ml.exe
                        rm -f "$CFILE"
                    else
                        exec clang "$@"
                    fi
                    """)
            )
        gcc_wrapper_path.chmod(0o755)

    def add_binutils_alike_symbolic_links():
        # Add symbolic links for the binutils-alike tools.
        # Tools not provided by LLVM: ranlib, size
        # Tenjin's LLVM ships `as` as a wrapper around `llvm-mc`
        #   because `llvm-as` is for assembling LLVM IR, not platform assembly.
        binutils_names = ["ar", "nm", "objcopy", "objdump", "readelf", "strings", "strip"]
        for name in binutils_names:
            src = hermetic.xj_llvm_root(localdir) / "bin" / f"llvm-{name}"
            dst = hermetic.xj_llvm_root(localdir) / "bin" / f"{name}"
            if not dst.is_symlink():
                os.symlink(src, dst)

        # These symbolic links follow a different naming pattern.
        symlinks = [("clang", "cc"), ("clang++", "c++")]
        if platform.system() != "Darwin":
            # On macOS, lld does not support -r (--relocatable) but the flag is used
            # by OCaml's build system, so we omit the symlink. This means that Clang
            # will use ld64.lld directly, but when OCaml invokes ld, it will get the
            # system's ld64 (non-LLD).
            symlinks.append(("lld", "ld"))
        for src, dst in symlinks:
            src = hermetic.xj_llvm_root(localdir) / "bin" / src
            dst = hermetic.xj_llvm_root(localdir) / "bin" / dst
            if not dst.is_symlink():
                os.symlink(src, dst)

    target = hermetic.xj_llvm_root(localdir)
    target_dir_existed = target.is_dir()
    if target.is_dir():
        shutil.rmtree(target)
        # In nuking the prior LLVM installation, we also lose the prior sysroot's extras.
        HAVE.note_removed("10j-bullseye-sysroot-extras")

    tarball_name = f"LLVM-{llvm_version}-{platform.system()}-{machine_normalized()}.tar.xz"
    if Path(tarball_name).is_file():
        # A local tarball was likely manually downloaded. Use it if we've got it.
        extract_tarball(Path(tarball_name), target, ctx="(llvm) ")
    else:
        url = f"https://github.com/Aarno-Labs/tenjin-build-deps/releases/download/{release}/{tarball_name}"
        download_and_extract_tarball(url, target, ctx="(llvm) ")

    match platform.system():
        case "Linux":
            provision_debian_sysroot()
            provision_clang_config_files(sysroot_path=f"<CFGDIR>/../{SYSROOT_NAME}")
        case "Darwin":
            xcrun_path = (
                subprocess.check_output(["xcrun", "--show-sdk-path"]).decode("utf-8").strip()
            )
            provision_clang_config_files(sysroot_path=xcrun_path)

    add_binutils_alike_symbolic_links()

    if target_dir_existed:
        # We must clean up any executables that dynamically linked against the old LLVM.
        # Tenjin's c2rust binary will be rebuilt on demand.
        sez("Cleaning up binaries linked against the prior LLVM version...", ctx="(c2rust) ")
        hermetic.run_cargo_in(["clean"], repo_root.find_repo_root_dir_Path() / "c2rust")

        # Upstream c2rust is not rebuilt automatically, so we need to do it here.
        upstream_c2rust_dir = hermetic.xj_upstream_c2rust(localdir)
        if upstream_c2rust_dir.is_dir():
            hermetic.run_cargo_in(["clean"], upstream_c2rust_dir)
            rebuild_10j_upstream_c2rust(upstream_c2rust_dir)

        # Likewise for Maki, which is a Clang plugin
        xj_hayroll_maki = hermetic.xj_hayroll_maki(localdir)
        if xj_hayroll_maki.is_dir():
            shutil.rmtree(xj_hayroll_maki / "build", ignore_errors=True)
            rebuild_10j_hayroll_maki(xj_hayroll_maki)

    update_10j_llvm_have(keyname, version, llvm_version)


def update_10j_llvm_have(keyname: str, version: str, llvm_version: str):
    out = subprocess.check_output([
        hermetic.xj_llvm_root(HAVE.localdir) / "bin" / "llvm-config",
        "--version",
    ])
    saw = out.decode("utf-8")
    if Version(saw) != Version(llvm_version):
        raise ProvisioningError(f"Expected LLVM version {llvm_version}, got {saw}.")
    HAVE.note_we_have(keyname, specifier=version)


#                COMMENTARY(pkg-config-paths)
# pkg-config embeds various configured paths into the binary.
# In particular, it embeds, via compiler flags during compilation,
# LIBDIR (via --prefix), PKG_CONFIG_PC_PATH, PKG_CONFIG_SYSTEM_INCLUDE_PATH,
# and PKG_CONFIG_SYSTEM_LIBRARY_PATH.
#
# So what we do, and ugh this leaves me feeling a little queasy, hurk, is...
# we embed very large fake paths into the pkg-config binary, and we call that
# binary "uncooked". Then we make a copy of the binary, and make in-place edits
# to have the embedded paths match those on the user's system. (The fake paths
# are large enough that they should accommodate whatever path the user has.)
def cook_pkg_config_within():
    def say(msg: str):
        sez(msg, ctx="(pkg-config) ")

    fifty = b"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    rilly = b"thisverylongpathistogiveusroomtooverwriteitlaterok"
    twohundredfifty = fifty + fifty + fifty + fifty + rilly
    path_of_unusual_size = b"/tmp/" + twohundredfifty + b"/" + twohundredfifty
    libdir = path_of_unusual_size + b"/prefix/lib"
    sysinc = path_of_unusual_size + b"/sysinc:/usr/include"
    syslib = path_of_unusual_size + b"/syslib:/usr/lib:/lib"
    pcpath = path_of_unusual_size + b"/lib/pkgconfig:" + path_of_unusual_size + b"/share/pkgconfig"
    nullbyte = b"\0"

    bindir = hermetic.xj_build_deps(HAVE.localdir) / "bin"

    uncooked = bindir / "pkg-config.uncooked"
    assert uncooked.is_file()
    cooked = bindir / "pkg-config"
    shutil.copy(uncooked, cooked)

    def replace_null_terminated_needle_in(haystack: bytes, needle: bytes, newstuff: bytes) -> bytes:
        # Make sure is has the embedded path/data we are expecting it to have.
        assert (needle + nullbyte) in haystack

        assert len(newstuff) <= len(needle)
        if len(newstuff) < len(needle):
            # Pad the newstuff with null bytes to match the length of needle.
            newstuff += b"\0" * (len(needle) - len(newstuff))

        assert len(newstuff) == len(needle)
        return haystack.replace(needle, newstuff)

    say("Cooking pkg-config...")
    with open(cooked, "r+b") as f:
        # Read the file into memory
        data = f.read()

        sysroot_usr = hermetic.xj_llvm_root(HAVE.localdir) / "sysroot" / "usr"
        newpcpath_lib = sysroot_usr / "lib" / "pkgconfig"
        newpcpath_shr = sysroot_usr / "share" / "pkgconfig"

        # Replace the placeholder strings with the actual paths.
        # Note that we set up the paths to include pkg-config's standard paths as backups,
        # in case the user is trying to compile against a library that isn't in the sysroot.
        data = replace_null_terminated_needle_in(
            data,
            sysinc,
            bytes(sysroot_usr / "include") + b":/usr/include",
        )
        data = replace_null_terminated_needle_in(
            data,
            syslib,
            bytes(sysroot_usr / "lib") + b":/usr/lib:/lib",
        )
        data = replace_null_terminated_needle_in(
            data, pcpath, bytes(newpcpath_lib) + b":" + bytes(newpcpath_shr) + b":/usr/lib:/lib"
        )
        data = replace_null_terminated_needle_in(
            data, libdir, bytes(hermetic.xj_build_deps(HAVE.localdir) / "lib")
        )

        # Write the modified data back to the file
        f.seek(0)
        f.write(data)
        f.truncate()
    say("... done cooking pkg-config.")

    assert path_of_unusual_size not in data, "Oops, pkg-config was left undercooked!"


def provision_10j_deps_with(version: str, keyname: str):
    match platform.system():
        case "Linux":
            filename = f"xj-build-deps_{machine_normalized()}.tar.xz"
            url = f"https://github.com/Aarno-Labs/tenjin-build-deps/releases/download/{version}/{filename}"
            target = hermetic.xj_build_deps(HAVE.localdir)
            if target.is_dir():
                shutil.rmtree(target)
            download_and_extract_tarball(url, target, ctx="(builddeps) ")
            cook_pkg_config_within()

        case "Darwin":
            # For macOS, we don't do hermetic build deps; instead, we rely on homebrew.
            # This bypasses the need to cook pkg-config, because it will have hardcoded
            # paths that are suitable for interoperation with homebrew.
            if shutil.which("brew") is None:
                raise ProvisioningError(
                    "Homebrew is not installed. Please install it from https://brew.sh"
                )

            if shutil.which("ninja") is None:
                subprocess.check_call(["brew", "install", "ninja"])

            if shutil.which("pkg-config") is None:
                subprocess.check_call(["brew", "install", "pkg-config"])

            if shutil.which("z3") is None:
                subprocess.check_call(["brew", "install", "z3"])

            subprocess.check_call(["brew", "install", "zlib"])

            # The other dependencies we need on Linux, like patch and make,
            # should have been provided already by Xcode Developer Tools.
            # Bubblewrap is Linux-only.

    HAVE.note_we_have(keyname, specifier=version)


def download_and_extract_tarball(
    tarball_url: str,
    target_dir: Path,
    ctx: str,
) -> None:
    """
    Downloads a compressed tar file from the given URL and extracts it to the target directory.

    Args:
        tarball_url (str): URL of the tarball file to download
        target_dir (str): Directory to extract contents to.
    """

    def say(msg: str):
        sez(msg, ctx)

    temp_file = None
    try:
        # Create a temporary file name for the download
        temp_file = os.path.basename(urlparse(tarball_url).path)

        say(f"Downloading {tarball_url}...")
        download(tarball_url, Path(temp_file))
    except Exception:
        # Clean up any temporary files if they exist
        if temp_file:
            if os.path.exists(temp_file):
                os.remove(temp_file)
        raise

    extract_tarball(Path(temp_file), target_dir, ctx)

    # Clean up the temporary file
    os.remove(temp_file)

    say(f"Download and extraction of {temp_file} completed successfully!")


# The extraction process is about twice as slow on macOS
# for clang+llvm versus the native bsdtar utility, but
# since this is a one-time cost it seems better to just
# avoid non-Python dependencies as much as we can.
def extract_tarball(
    tarball_path: Path, initial_target_dir: Path, ctx: str, time_estimate="a few seconds"
) -> Path:
    """
    Extracts the given tarball into (or within) the target directory.

    If the tarball unpacks a single directory with the same name as the tarball
    (minus the suffix), the contents of that directory will be moved up a level,
    and the empty directory will be removed.

    Returns the path to the directory that contains the unpacked contents.
    """

    def say(msg: str):
        sez(msg, ctx)

    def is_empty_dir(path: Path) -> bool:
        if not path.is_dir():
            return False

        is_empty = True
        for item in path.iterdir():
            is_empty = False
            break
        return is_empty

    def choose_target_dir(initial_target_dir: Path) -> tuple[Path, str]:
        # Check if the tarball unpacks a single directory with the same name as the tarball
        def select_tarball_suffix(filename: str) -> str:
            if filename.endswith(".tar.xz"):
                return ".tar.xz"
            elif filename.endswith(".tar.gz"):
                return ".tar.gz"
            elif filename.endswith(".tgz"):
                return ".tgz"
            elif filename.endswith(".tar.bz2"):
                return ".tar.bz2"
            elif filename.endswith(".tbz"):
                return ".tbz"
            raise ValueError(f"Unknown tarball suffix for URL: {filename}")

        suffix = select_tarball_suffix(tarball_path.name)
        tarball_basename = tarball_path.name.removesuffix(suffix)

        target_dir_preexisted = initial_target_dir.is_dir()
        if target_dir_preexisted and not is_empty_dir(initial_target_dir):
            # If the target directory already existed, and is not empty,
            # we'll unpack the tarball into a new directory inside it.

            final_target_dir = initial_target_dir / tarball_basename
            final_target_dir.mkdir(parents=True, exist_ok=True)
        else:
            final_target_dir = initial_target_dir

        return final_target_dir, tarball_basename

    if time_estimate is not None:
        say(f"This will take {time_estimate}...")

    final_target_dir, tarball_basename = choose_target_dir(initial_target_dir)

    if final_target_dir != initial_target_dir:
        say(f"Extracting to subdirectory {final_target_dir}...")
    else:
        say(f"Extracting to {initial_target_dir}...")

    # Create target/parent directory if it doesn't exist
    initial_target_dir.mkdir(parents=True, exist_ok=True)

    # Extract the compressed tar file
    with tarfile.open(str(tarball_path), "r:*") as tar:
        tar.extractall(path=final_target_dir)

    if time_estimate is not None:
        say(f"Extraction of {tarball_path.name} completed successfully!")

    # For example, we have foo-bar.tar.gz, and unpack it into blah/;
    #   then if we find blah/foo-bar/, we trim out the foo-bar part.
    final_dir_contents = list(final_target_dir.iterdir())
    replicated_tarball_name = final_dir_contents == [final_target_dir / tarball_basename]
    # Likewise, if we find blah/blah/, we trim out the middle blah part.
    replicated_target_basename = final_dir_contents == [final_target_dir / final_target_dir.name]
    if replicated_tarball_name or replicated_target_basename:
        extracted_path = final_dir_contents[0]
        # The tarball unpacks a single directory, so move its contents up a level
        for item in extracted_path.iterdir():
            shutil.move(str(item), str(final_target_dir))

        # Remove the now-empty directory
        extracted_path.rmdir()

    return final_target_dir
