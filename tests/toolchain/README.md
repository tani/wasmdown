# Generated WASI guest (toolchain evidence)

`wasi-guest/` is a Rust command guest built with the pinned toolchain in
`wasi-guest/rust-toolchain.toml` (nightly-2026-11-01, `wasm32-wasip3`).
The Rust target's std imports the WASI 0.3 line as `@0.3.0`; the runtime
maps `0.3.x` patch versions onto the vendored `@0.3.2` providers.

```sh
tools/setup-wasi-toolchain.sh
make build-toolchain-guest   # writes tests/fixtures/wasi/toolchain-guest.wasm + hash
make test-toolchain-guest    # both backends, exact stdout
```
