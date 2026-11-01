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

## Unsupported by design (not pending)

`wasi:http` is **excluded permanently by user decision (2026-11-02)**. It was
removed in M17 (M16 slice 5) and is recoverable from git history; it is not an
implementation gap, is not planned, and will not be restored. The vendored
`wasi-wit-0.3.2/http/http.wit` remains **reference data only** — it is kept so
the vendored tree stays byte-identical to the release asset, and no provider,
world, adapter or test may be derived from it as a deliverable.

Excluded vendored declarations, verified against
`specs/wasi/wasi-wit-0.3.2/http/http.wit`:

| Declaration | Kind | Status |
| --- | --- | --- |
| `wasi:http/types@0.3.2` | interface | unsupported by design (vendored reference data only) |
| `wasi:http/handler@0.3.2` | interface | unsupported by design (vendored reference data only) |
| `wasi:http/client@0.3.2` | interface | unsupported by design (vendored reference data only) |
| `wasi:http/service` | world | unsupported by design |
| `wasi:http/middleware` | world | unsupported by design |

There is no `outgoing-handler` / `incoming-handler` in the pinned 0.3.2
release; the outgoing client is `client.send` and the incoming service is
`handler.handle` inside the `service`/`middleware` worlds.

A guest or embedder that imports `wasi:http` is refused at link time with this
exact message:

```
wasi:http is not supported by this runtime by design (removed in M17); the pinned HTTP WIT is reference data only. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.
```

### Network communication: `wasi:sockets` (excluded)

Network communication — **TCP, UDP and DNS** — is **excluded permanently by
user decision (2026-11-02)**, by the same decision that removed `wasi:http`.
It is not an implementation gap, is not planned, and will not be restored.
The vendored `wasi-wit-0.3.2/sockets/sockets.wit` remains **reference data
only**: it is kept so the vendored tree stays byte-identical to the release
asset, and no guest-visible provider, world or adapter may be derived from it
as a deliverable.

Excluded vendored declarations, verified against
`specs/wasi/wasi-wit-0.3.2/sockets/sockets.wit`:

| Declaration | Kind | Status |
| --- | --- | --- |
| `wasi:sockets/types@0.3.2` | interface (`error-code`, address types, the `tcp-socket` and `udp-socket` resources) | unsupported by design (vendored reference data only) |
| `wasi:sockets/ip-name-lookup@0.3.2` | interface (`resolve-addresses`, i.e. DNS) | unsupported by design (vendored reference data only) |
| `wasi:sockets/imports@0.3.2` | world | unsupported by design |

A **0.3-line** component that *declares* an import from `wasi:sockets` — any
0.3.x interface/version — is refused at link time with this exact message (the
separate 0.2 facade is unaffected by this check):

```
Network communication (TCP, UDP and DNS) is not supported by this design; there is no wasi:sockets provider for guests. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.
```

The pinned `wasi:cli/command` world imports `wasi:sockets/types@0.3.2` and
`wasi:sockets/ip-name-lookup@0.3.2` itself (the `command` world in
`specs/wasi/wasi-wit-0.3.2/cli/cli.wit`). A 0.3.2 sockets provider is therefore retained
inside the runtime **only** to compose that pinned world's own imports; it is
an implementation detail, not a guest surface, and it is not evidence of
network support. There is no CLI network authority either: `--net`,
`--listen` and `--dns` are refused as usage errors (exit 2) before any
authority is built.

The declared support profile is therefore `conformant for the declared WASI
0.3.2 profile (wasi:http and wasi:sockets excluded)` at most; an unqualified
"full WASI 0.3.2" label is not claimed (see `docs/design/PLAN.md` §2.1).

## Verification

```sh
curl -sL -o /tmp/wasi-wit-0.3.2.tar.gz \
  https://github.com/WebAssembly/WASI/releases/download/v0.3.2/wasi-wit-0.3.2.tar.gz
sha256sum /tmp/wasi-wit-0.3.2.tar.gz
# 2e918e047c6ad251a44b4d1141359495a27219058fd7090f8b8142f6bc0ec311
tar xzf /tmp/wasi-wit-0.3.2.tar.gz -C specs/wasi/
```

## Policy (docs/design/PLAN.md §2)

Vendor exact stable WIT snapshots; never float a main-branch WIT. Version
dispatch is by the package's `@0.3.2` suffix in the import name (e.g.
`wasi:clocks/monotonic-clock@0.3.2`). `@unstable(feature = ...)` interfaces
are feature-gated and not enabled by default (`clocks-timezone` is the
first such gate).

## Additional pinned trees (Preview1 + WASI 0.2)

Added for the Preview1 + WASI 0.2 target. All of it is produced and
verified by `tools/vendor-wasi-specs.sh` (use `--check` to re-hash the
vendored tree without writing). The 0.3.2 tree above is untouched.

### Preview1 (`specs/wasi/preview1/`)

- Source: `WebAssembly/WASI` commit `fae981bae14809d91f9bc2d63852d461f331d161`
- `witx/wasi_snapshot_preview1.witx` sha256 `30e31a9b830d3fad998701fc9439a5506af4dbb8f01202854c8af46d436de679`
- `witx/typenames.witx` sha256 `980ef8a0f8d345f65d0c7ce07f01981174712ae0c6c44b3c250cbb2b0c752dc5`
- `docs.md` sha256 `ac9e99351efd42af854d23c6cb858289a51ea485ab3c3923fa5837525e6a06c6`
- `INVENTORY.md` — generated by `tools/wasi-inventory.py` from the WITX files
  (functions, signatures, layouts, errnos, rights); the implementation
  registry is never the denominator.

### WASI 0.2 (`specs/wasi/wasi-wit-0.2.*/`)

Supported exact package versions: **0.2.0 … 0.2.12**, default 0.2.12.

- **v0.2.10 … v0.2.12**: the official release asset `wasi-wit-<v>.tar.gz`,
  vendored verbatim. SHA-256 0.2.10 `e6673a0d…c3a31`, 0.2.11 `0607082a…c6501`,
  0.2.12 `2b0e47bc…05992`.
- **v0.2.0 … v0.2.9**: those tags predate the release-asset workflow. They are
  vendored from the immutable tag source tree (commit pinned per version in
  `tools/vendor-wasi-specs.sh`) with the layout normalized to
  `<package>/<interface>.wit`; the file bytes are verbatim. The tag tree moved
  over the range (`preview2/` → `wasip2/` → `proposals/<pkg>/wit/`).
- `specs/wasi/index.txt` is the generated `(version, package) -> path` index
  the runtime reads. Package lookup uses this index only: never directory-name
  guessing, never a network query at runtime.

### Fixture inputs (development-only)

- `wasi_snapshot_preview1.command.wasm` / `.reactor.wasm` from Wasmtime
  **v50.0.1** (sha256 `0b4fe490…132a3c4` / `6d52112d…25671f`), fetched by
  `tools/build-wasi-fixtures.sh` into an ignored cache.
- SQLite amalgamation 3.54.1 (`0a9c8e34…ad1f9`), built with the pinned Nix Zig.
