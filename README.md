# WASMDOWN

A WebAssembly runtime written in Common Lisp.

Binary and text front ends, validation, an interpreter, an SBCL-compiling
backend, the Component Model, and WASI hosts — with **no third-party Lisp
dependencies**.

> **Status:** the engine/legacy surface is `partial`; the declared WASI 0.3.2
> profile is `conformant`. See [`docs/status.md`](docs/status.md) for the
> per-area matrix, limits, and exclusions.

## Quick start

Requires **SBCL** (developed against 2.6.4). Python 3 is needed only for the
WASI behaviour/network lanes; Nix only to rebuild pinned WASI fixtures.

```sh
make test-unit                         # 761 tests, ~8 s
sbcl --script tools/wasmdown.lisp help
```

Validate and run a core module:

```sh
sbcl --script tools/wasmdown.lisp validate add.wasm
sbcl --script tools/wasmdown.lisp run add.wasm --invoke add 42 13
```

Run a WASI guest (the SQLite fixture is built with `make wasi-fixtures`):

```sh
printf 'SELECT 6*7;\n' | sbcl --script tools/wasmdown.lisp run \
  tests/fixtures/wasi/sqlite3-p1.wasm --wasi -- -batch :memory:
# 42
```

Embed it from Lisp:

```commonlisp
(let* ((module   (wasmdown:read-module #P"add.wasm"))
       (checked  (wasmdown:validate-module module))
       (instance (wasmdown:instantiate checked)))
  (wasmdown:invoke instance "add" 42 13))   ; => (55)
```

## What is implemented

| Area | State |
| --- | --- |
| Core binary/text formats and `.wast` runner | Implemented; stateful script runner with failure phases |
| Core validation and interpreter | Implemented for the declared Core 3.0 scope |
| Compiled backend | SBCL `COMPILE`; every pinned core and component command agrees with the interpreter |
| Component Model | Binary/text decoding, validation, linking, canonical ABI, resources, async |
| WASI 0.3.2 | Declared profile: `clocks`, `random`, `cli`, `filesystem`, `sockets`, `http` |
| WASI 0.2 / Preview 1 | Legacy compatibility facades; all 46 Preview 1 imports |
| Threads / atomics | Opt-in extension via `--threads` (real SBCL threads) |
| Out of scope | Annotations, unratified proposals, JS/browser embedding, AOT/FASL cache |

Network access is declared, never ambient:

```sh
sbcl --script tools/wasmdown.lisp run net-guest.wasm \
  --wasi --net 127.0.0.1:8080 --dns example.test
```

Serve a `wasi:http/service` component (the URL is the first stderr line):

```sh
sbcl --script tools/wasmdown.lisp serve service.wasm --wasi --addr 127.0.0.1:8080
```

## Verification

Every claim is backed by a measured lane. Headline results:

- unit: **761 / 761**
- core spec suite: **21,187 pass / 0 fail / 73 unsupported** (annotations only)
- component suite: **1,702 / 1,702**
- upstream WASI testsuite: **127 pass / 0 fail / 0 skip**
- backend parity: **21,260 / 21,260** commands agree

See [`docs/conformance/`](docs/conformance/README.md) for the lanes and
[`docs/status.md`](docs/status.md) for the exact limits.

## Documentation

| I want to… | Read |
| --- | --- |
| Understand the pipeline and layout | [`docs/architecture.md`](docs/architecture.md) |
| Check support, limits, and exclusions | [`docs/status.md`](docs/status.md) |
| Use the CLI or embed the Lisp API | [`docs/usage/cli.md`](docs/usage/cli.md), [`docs/usage/lisp-api.md`](docs/usage/lisp-api.md) |
| Run WASI guests and configure capabilities | [`docs/usage/wasi.md`](docs/usage/wasi.md) |
| Build, test, and hack on the runtime | [`docs/development.md`](docs/development.md) |
| Read the design contract and history | [`docs/design/PLAN.md`](docs/design/PLAN.md), [`docs/history.md`](docs/history.md) |

Full index: [`docs/README.md`](docs/README.md).

## License

Not chosen yet — `wasmdown.asd` declares `:licence "TBD"`.
