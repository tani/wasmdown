# WASI WIT vendoring manifest

## Snapshot

- **Release**: `WebAssembly/WASI` tag **v0.3.2**
- **Published**: 2026-10-13T13:23:18Z
- **Asset**: `wasi-wit-0.3.2.tar.gz` (25,493 bytes)
- **URL**: https://github.com/WebAssembly/WASI/releases/download/v0.3.2/wasi-wit-0.3.2.tar.gz
- **SHA-256** (of the downloaded tarball):
  `2e918e047c6ad251a44b4d1141359495a27219058fd7090f8b8142f6bc0ec311`

## Contents (verbatim from the release asset)

```
wasi-wit-0.3.2/cli/cli.wit                      256 lines
wasi-wit-0.3.2/cli/deps/clocks.wit
wasi-wit-0.3.2/cli/deps/filesystem.wit
wasi-wit-0.3.2/cli/deps/random.wit
wasi-wit-0.3.2/cli/deps/sockets.wit
wasi-wit-0.3.2/clocks/clocks.wit                161 lines
wasi-wit-0.3.2/filesystem/filesystem.wit        575 lines
wasi-wit-0.3.2/filesystem/deps/clocks.wit
wasi-wit-0.3.2/http/http.wit                    510 lines
wasi-wit-0.3.2/http/deps/cli.wit
wasi-wit-0.3.2/http/deps/clocks.wit
wasi-wit-0.3.2/http/deps/random.wit
wasi-wit-0.3.2/random/random.wit                107 lines
wasi-wit-0.3.2/sockets/sockets.wit              839 lines
wasi-wit-0.3.2/sockets/deps/clocks.wit
```

The `*/deps/*.wit` files are the release's own copies of the depended-on
interfaces (the release ships each package standalone with its deps
inlined as separate files); they are retained verbatim so the vendored
tree is byte-identical to the release asset.

## Verification

```sh
curl -sL -o /tmp/wasi-wit-0.3.2.tar.gz \
  https://github.com/WebAssembly/WASI/releases/download/v0.3.2/wasi-wit-0.3.2.tar.gz
sha256sum /tmp/wasi-wit-0.3.2.tar.gz
# 2e918e047c6ad251a44b4d1141359495a27219058fd7090f8b8142f6bc0ec311
tar xzf /tmp/wasi-wit-0.3.2.tar.gz -C specs/wasi/
```

## Policy (PLAN.md §2)

Vendor exact stable WIT snapshots; never float a main-branch WIT. Version
dispatch is by the package's `@0.3.2` suffix in the import name (e.g.
`wasi:clocks/monotonic-clock@0.3.2`). `@unstable(feature = ...)` interfaces
are feature-gated and not enabled by default (`clocks-timezone` is the
first such gate).
