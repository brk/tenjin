# Note: the keys in this dict are not command names, or file names,
# just arbitrary labels for the things we are tracking.
WANT = {
    "10j-llvm": "18.1.8@tenjin-llvm-4256d1483",
    "10j-opam": "2.3.0",
    "10j-dune": "3.19.0",
    "10j-ocaml": "5.2.0",
    "10j-cmake": "3.31.7",
    "10j-bullseye-sysroot-extras": "rev-6b5fc80e3",
    "10j-build-deps": "rev-6b5fc80e3",
    # Keep in sync with the version in `xj-improve-multitool/rust-toolchain.toml`.
    "10j-xj-improve-multitool-toolchain": "nightly-2025-03-03",
    "10j-xj-default-rust-toolchain": "1.88.0",
    "10j-reference-c2rust-tag": "v0.20.0",
    "uw-harvest-maki": "fa9206c600fb5209984f5b83b996d1966e8da1bc",
}

# Subdirectory of hermetic.xj_llvm_root()
SYSROOT_NAME = "sysroot"

if __name__ == "__main__":
    # This is a separate script from provisioning.py so that it can be run
    # with a stock Python interpreter, without any third-party modules.
    import sys
    import platform

    match sys.argv:
        case [_, "ocaml-cache-key"]:

            def piece(k):
                return f"{k}-{WANT['10j-' + k]}"

            ocamlparts = ";".join(piece(k) for k in "ocaml opam dune".split())
            print(";".join([platform.system(), platform.machine(), ocamlparts]))
        case _:
            pass
