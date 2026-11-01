# WASMDOWN

A WebAssembly runtime written in Common Lisp. Binary and text front ends,
validation, a reference interpreter, a baseline SBCL code generator, the
Component Model (canonical ABI, resources, async), and WASI hosts — with
**no third-party Lisp dependencies**.

> **Status: `partial`.** Every subsystem listed below is implemented and
> gated by its own test lane; the engine deliberately omits threads/atomics,
> the FASL cache, and a native optimizer. See [`docs/status.md`](docs/status.md)
> for the per-area matrix and the exact limits.

## At a glance

| Area | State |
| --- | --- |
| Binary decoder / encoder | Full: bounds-checked reader, canonical minimal writer, round-trip tested |
| Text format (`.wat`) and `.wast` scripts | Full: parser, stateful script runner, failure-phase classification |
| Validation | Full for the declared engine scope, including unreachable polymorphism |
| Interpreter (default backend) | Full: exact i32/i64/f32/f64 semantics, v128, GC, EH, tail calls, multi-memory, memory64 |
| Codegen backend (`baseline`) | Partial: SBCL `COMPILE` of emitted Lisp; no `return_call*`, compiled core-suite parity incomplete |
| Component Model | Full lane green: binary + text, validation, instantiation/linking, canonical ABI, resources, async scheduling |
| WASI Preview 1 | Full `wasi_snapshot_preview1` surface (46 imports) |
| WASI 0.3.2 providers | `clocks`, `random`, `cli`, `filesystem`, `sockets` under an explicit capability model |
| WASI 0.2 component facade | 27 of 31 stable interfaces bound at exact release names |
| Threads / atomics, FASL cache, optimized or AOT backend | Not implemented |
| Annotations proposal | Out of scope |

## Repository structure

```mermaid
graph TD
  ROOT["wasmdown"] --> SRC["src/ — 22 files, one per subsystem"]
  ROOT --> TESTS["tests/"]
  ROOT --> TOOLS["tools/"]
  ROOT --> DOCS["docs/"]
  ROOT --> SPECS["specs/ — vendored specs and pins"]
  ROOT --> BUILD["wasmdown.asd + Makefile"]
  SRC --> FRONT["frontends<br/>binary · binary-encode · text · wast"]
  SRC --> CORE["core engine<br/>types · validate · runtime · interp · codegen"]
  SRC --> COMP["component model<br/>component · component-text · component-runtime · wit"]
  SRC --> HOST["WASI hosts<br/>wasi · wasi-preview1 · wasi-preview2"]
  SRC --> BASE["shared base<br/>packages · conditions · numops · simd · api · cli"]
  TESTS --> T_U["unit and integration pins"]
  TESTS --> T_L["spec-core/ · component/ · fixtures/ — lane corpora"]
  TOOLS --> X_L["lane runners · inventory · cost bench · fixture builders"]
  DOCS --> D_A["architecture · status · usage/ · development"]
  DOCS --> D_C["conformance/ — generated reports"]
  DOCS --> D_H["design/PLAN · adr/ · history"]
```

`tests/` and `tools/` feed the lanes whose generated results land in
`docs/conformance/`; the module-by-module pipeline diagram is in
[`docs/architecture.md`](docs/architecture.md).

## Quick start

Requires **SBCL** (developed against 2.6.4); Python 3 only for the WASI
behaviour lane, Nix only to rebuild the pinned WASI fixtures.

```sh
make test-unit                  # unit + integration suite (663 tests, ~8 s)
sbcl --script tools/wasmdown.lisp help
```

Validate, inspect, and run a core module:

```sh
sbcl --script tools/wasmdown.lisp validate add.wasm
sbcl --script tools/wasmdown.lisp inspect  add.wasm
sbcl --script tools/wasmdown.lisp run      add.wasm --invoke add 42 13
```

Run a WASI guest against the pinned SQLite fixture (built once with
`make wasi-fixtures`, which needs the pinned Nix toolchain):

```sh
printf 'SELECT 6*7;\n' | sbcl --script tools/wasmdown.lisp run \
  tests/fixtures/wasi/sqlite3-p1.wasm --wasi -- -batch :memory:
# 42
```

From Lisp:

```commonlisp
(let* ((module   (wasmdown:read-module #P"add.wasm"))
       (checked  (wasmdown:validate-module module))
       (instance (wasmdown:instantiate checked)))
  (wasmdown:invoke instance "add" 42 13))   ; => (55)
```

## Verification

Every lane is measured, not asserted. Latest numbers (2026-11-01):

| Lane | Kind | Measured | Docs |
| --- | --- | --- | --- |
| `make test-unit` | gate | 663 pass / 0 fail | — |
| `make test-spec-core` | baseline | 21 187 pass / 0 fail / 73 unsupported of 21 260 | [`core.md`](docs/conformance/core.md) |
| `make test-component` | gate | 1 702 pass / 0 fail / 0 unsupported | [`component.md`](docs/conformance/component.md) |
| `make report-component-baseline` | report | 1 702 pass / 0 fail under `:baseline` | `tests/component-baseline/component.md` |
| `make test-backend-parity` | gate | 21 260 / 21 260 core commands agree across both backends | [`backends.md`](docs/conformance/backends.md) |
| `make test-wasi` | gate | 450/450 cases over 30 fresh-process runs | [`wasi.md`](docs/conformance/wasi.md) |
| `make test-upstream` | discovery | aggregated proposal corpus, never a gate | [`upstream.md`](docs/conformance/upstream.md) |
| `make report-backends-bench` | report | per-backend cost medians | [`performance.md`](docs/conformance/performance.md) |

## Documentation

Full index: **[`docs/`](docs/README.md)**.

| I want to… | Read |
| --- | --- |
| Understand the pipeline and module layout | [`docs/architecture.md`](docs/architecture.md) |
| Check what is supported, limited, or planned | [`docs/status.md`](docs/status.md) |
| Use the CLI | [`docs/usage/cli.md`](docs/usage/cli.md) |
| Embed the runtime from Lisp | [`docs/usage/lisp-api.md`](docs/usage/lisp-api.md) |
| Run WASI guests / configure capabilities | [`docs/usage/wasi.md`](docs/usage/wasi.md) |
| Interpret an error or trap | [`docs/usage/errors.md`](docs/usage/errors.md) |
| Reproduce the conformance numbers | [`docs/conformance/`](docs/conformance/README.md) |
| Build, test, and hack on the runtime | [`docs/development.md`](docs/development.md) |
| Read the design contract and milestones | [`docs/design/PLAN.md`](docs/design/PLAN.md) |
| See why a decision was made | [`docs/adr/`](docs/adr/README.md) |
| Read the engineering history | [`docs/history.md`](docs/history.md) |

## License

Not yet chosen — `wasmdown.asd` declares `:licence "TBD"`.
