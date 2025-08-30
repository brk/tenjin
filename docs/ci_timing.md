# Notes on GitHub Actions CI timing/performance

* Python-only checks usually complete in 8 seconds.
  - Half of that time is to setup, clone, & provision `uv`.
  - Half is cold-start time for `10j check-py`.

* Rust stuff is about 3 minutes
  - 1 minute to provision 10j & Rust
  - 2 minutes to build c2rust and associated tests
  - Saving of Rust cache stuff can take several minutes, but
    we have things configured to not pay this cost on PR/test branches.

* Provisioning OCaml needs to build OCaml from source
  - Uncached, this takes about five minutes (for OCaml, `opam`, and `dune`).
  - Caching reduces this to about twenty seconds.