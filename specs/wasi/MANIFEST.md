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

## Network surface (declared supported)

The declared WASI 0.3.2 profile includes **all network features** in the
pinned release. The previously recorded exclusion (2026-11-02) is superseded
on the `feat/http` branch: `wasi:http` and `wasi:sockets` are no longer
reference-only data; their vendored WIT is the normative contract for the
providers, worlds, adapters and tests.

Supported vendored declarations, verified against
`specs/wasi/wasi-wit-0.3.2/http/http.wit`:

| Declaration | Kind | Status |
| --- | --- | --- |
| `wasi:http/types@0.3.2` | interface | supported |
| `wasi:http/handler@0.3.2` | interface | supported |
| `wasi:http/client@0.3.2` | interface | supported |
| `wasi:http/service` | world | supported (`serve-component`; incoming `handler.handle`) |
| `wasi:http/middleware` | world | partial: the world's shape is vendored and resolvable, but composition with a host-supplied `next` handler is not covered by the pinned suite and is not claimed |

There is no `outgoing-handler` / `incoming-handler` in the pinned 0.3.2
release; the outgoing client is `client.send` and the incoming service is
`handler.handle` inside the `service` world. The `service` direction is
exercised by `wasmdown serve`; the `middleware` world's host-supplied `next`
composition remains unverified and is therefore marked partial above, not
claimed as supported.

### Network communication: `wasi:sockets`

TCP, UDP and DNS are part of the declared profile. Supported vendored
declarations, verified against
`specs/wasi/wasi-wit-0.3.2/sockets/sockets.wit`:

| Declaration | Kind | Status |
| --- | --- | --- |
| `wasi:sockets/types@0.3.2` | interface (`error-code`, address types, the `tcp-socket` and `udp-socket` resources) | supported |
| `wasi:sockets/ip-name-lookup@0.3.2` | interface (`resolve-addresses`, i.e. DNS) | supported |
| `wasi:sockets/imports@0.3.2` | world | supported |

The pinned `wasi:cli/command` world imports `wasi:sockets/types@0.3.2` and
`wasi:sockets/ip-name-lookup@0.3.2` itself (the `command` world in
`specs/wasi/wasi-wit-0.3.2/cli/cli.wit`). Guest-declared 0.3-line imports of
these interfaces are therefore admitted by the declared profile, and the CLI
network authorities (`--net`, `--listen`, `--dns`) are part of the supported
capability model rather than usage errors.

The support declaration now covers the full vendored WASI 0.3.2 surface.
Conformance labels still follow the recorded gates in `docs/status.md` and
`docs/design/PLAN.md`; this manifest defines scope, not a claim that every
gate has already been measured on this branch.

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
