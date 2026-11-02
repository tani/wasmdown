# Development

How to build, test and change WASMDOWN. Lane definitions and measured
results live in [`conformance/`](conformance/README.md); this page covers
the toolchain, the Makefile, the layout, and the conventions a change has
to follow.

## Prerequisites

| Need | For | Note |
| --- | --- | --- |
| SBCL | everything | Developed against 2.6.4. The 64 MiB default memory ceiling is an implementation limit — see [ADR-005](adr/ADR-005-memory-ceiling.md). |
| Python 3 | `test-wasi-*`, `test-upstream`, `tools/wasi-inventory.py` | Standard library only; no packages to install. |
| bash, `curl`, `unzip`, `sha256sum` | `test-component-census`, `wasi-fixtures` | Present on any normal Linux/macOS host. |
| Nix | rebuilding WASI fixtures only | Pins `zig` 0.17.0 and `wasm-tools` 1.262.0 in [`tools/wasi-toolchain.nix`](../tools/wasi-toolchain.nix); `wasmtime` 50.0.0 is listed there as a comparison executor only. |
| Rust nightly 2026-11-01 + `wasm32-wasip3`, wasm-tools 1.263.0 | generated toolchain guest | Installed under `.cache/` by `make setup-wasi-toolchain`; `make build-toolchain-guest` and `make test-toolchain-guest`. |
| wasi-sdk 34.0, curl 8.22.0 | curl-as-WASM network test | Installed/built under `.cache/` by `make setup-curl-wasm`; `make test-curl-wasm` serves a loopback file and asserts the body. |

`make test-unit` needs SBCL and nothing else. The WASI lanes drive the
**real CLI** (`tools/wasmdown.lisp`) as a subprocess, so they measure the
shipped path rather than an internal entry point; the core, component and
upstream lanes drive the WAST runner under SBCL.

## Make targets

| Target | Runs | Kind | Notes |
| --- | --- | --- | --- |
| `all` | `test` | utility | Default target; alias of `test`. |
| `verify` | unit, component, parity gate, inventory check, `test-wasi`, P3 lane, P3 fixture hash | gate | Reproducible quick gate: the deterministic lanes that must be green on any change (~2.5 min). Compiled large-guest lanes stay report-only. |
| `test` | `test-unit` | utility | Alias. |
| `test-unit` | `sbcl --script tools/run-tests.lisp` | gate | Units and integration tests in one process: ~663 tests, ~8 s. Non-zero exit on any failure. |
| `test-spec-core` | `sbcl … --script tools/run-spec-core.lisp` | baseline | Pinned core `.wast` suite through the stateful WAST runner. Regenerates [`conformance/core.md`](conformance/core.md) and `tests/spec-core-results.jsonl`. Runs in ~3 s; a slow or hanging run is a bug. |
| `test-component` | `sbcl … --script tools/run-component-suite.lisp` | gate | Vendored component suite, `tests/component/{binary,validation,linking,values,async,resources}` minus `nyi.txt`. Regenerates [`conformance/component.md`](conformance/component.md); exits non-zero on any fail, skip, timeout, harness error, or census drift. |
| `test-component-census` | `bash tools/run-component-census.sh $(RUNS)` | gate | Repeats the component lane in fresh processes and fails unless every run is green at the same command denominator. `RUNS` defaults to 30 (`make test-component-census RUNS=5`). Logs land in `tmp-census/`. |
| `report-component-compiled` | `sbcl … --script tools/run-component-suite.lisp --backend compiled --mode report` | report | The same component corpus with every defined core function compiled before its start section. Writes `tests/component-compiled/` and `tests/component-compiled-results.jsonl`; never overwrites the released report. |
| `test-backend-parity` | `sbcl … --script tools/run-backend-parity.lisp --mode gate` | gate | Runs the pinned core scripts through both backends; exits non-zero on any divergence, compile refusal, timeout/harness error, or command-count drift. Gate runs keep their artifacts in `tests/backend-parity/`. |
| `report-backend-parity` | `sbcl … --script tools/run-backend-parity.lisp --mode report --commit` | report | Regenerates [`conformance/backends.md`](conformance/backends.md), `tests/backend-parity-results.jsonl` and the summary. |
| `report-backend-inventory` | `python3 tools/backend-inventory.py` | report | Regenerates [`conformance/inventory.md`](conformance/inventory.md) and `tests/backend-inventory.json` from the runtime's registration tables vs the pinned census. |
| `test-backend-inventory` | `python3 tools/backend-inventory.py --check` | check | Fails when the committed inventory is stale. |
| `report-backends-bench` | `sbcl … --script tools/bench-backends.lisp --repeat 3 --commit --json tests/bench-backends.json` | report | Per-backend decode / validate / instantiate(+compile) / run medians into [`conformance/performance.md`](conformance/performance.md). |
| `component-encode` | `sbcl --script tools/component-encode.lisp IN.wat OUT.wasm` | tool | Minimal component text→binary encoder over the parser IR (types, imports/exports, aliases, canon lift/lower + async builtins, core modules/instances, component instances). Guarded by `wasi/component-text-encoder-roundtrip`. |
| `record-release` | parity + component (both backends) + WASI lanes + P3 + bench + inventory | report | Regenerates every artifact the release matrix reads. The compiled P2 row is separate (`make record-wasi-compiled`, ~37 min). |
| `record-p2` / `record-p3` / `record-p3-compiled` | `tools/run-wasi-suite.py --abi … --output-dir tests/wasi-results/…` | report | Canonical output directories the release matrix checks. |
| `record-wasi-compiled` | P2 lane, `--backend compiled --heap-mb 8192` | report | The expensive (~37 min) compiled WASI row. |
| `verify-release` | `python3 tools/check-release-matrix.py` | gate | Every release-matrix row present and green, each naming a tested source commit (HEAD or an ancestor); the summary lists every distinct tested commit so a lagging compiled row is visible. Stdout-only P1 is listed as `n/a` with its gate named. |
| `test-property` | `sbcl … --script tools/fuzz-backends.lisp --iterations $(ITERATIONS)` | gate | Differential fuzz: generated core modules run under both backends; results, trap kinds and observable memory must agree. A failure prints its seed (`--iterations 1 --seed N`). `ITERATIONS` defaults to 2000 (~15 s). |
| `test-p1-repeat` | `python3 tools/wasi-preview1-acceptance.py` × `$(RUNS)` | gate | Repeats the P1 acceptance in fresh processes. `RUNS` defaults to 3; interpret mode (the compiled form is report evidence). |
| `test-wasi-preview1` | `python3 tools/wasi-preview1-acceptance.py` | gate | Preview1 acceptance: real CLI over the pinned SQLite shell fixtures, exact stdout/status, plus the host-side denial cases. Builds nothing. `--backend compiled [--heap-mb N]` runs the same cases with compiled guest cores (report evidence, ~200 s per fresh process). |
| `test-wasi-preview2` | `python3 tools/run-wasi-suite.py --abi preview2` | gate | WASI 0.2 behaviour: one fresh SBCL guest process per case. |
| `test-wasi-preview3` | `sbcl --script tools/build-p3-fixtures.lisp` then `python3 tools/run-wasi-suite.py --abi preview3` | gate (explicit) | Hand-authored @0.3.2 command fixtures; the generated Rust guest is separate (`test-toolchain-guest`). Both backends. Not in `--abi all`. |
| `p3-fixtures` | `sbcl --script tools/build-p3-fixtures.lisp` | utility | Writes `tests/fixtures/wasi/p3-guest.wasm` and `P3-CHECKSUMS`; `--check` verifies the recorded hash. |
| `test-wasi` | `python3 tools/run-wasi-suite.py --abi all` | gate | Both released ABIs (`preview1` + `preview2`); `--abi preview3` is explicit-only and fails on the missing pinned @0.3.2 guest instead of skipping; honours `WASMDOWN_API_MODE=compiled` (the runner's `--backend` flag), which really compiles the guest cores and is much slower. Regenerates [`conformance/wasi.md`](conformance/wasi.md) only with `--commit-summary`, so a gate run cannot clobber the committed artifact. |
| `test-upstream` | `python3 tools/run-upstream-testsuite.py` | discovery | Fetches the aggregated testsuite mirror at an exact commit and measures it; writes `tests/upstream-results.jsonl` and [`conformance/upstream.md`](conformance/upstream.md). Never a gate. |
| `setup-wasi-toolchain` / `build-toolchain-guest` / `test-toolchain-guest` | install the pinned Rust wasm32-wasip3 toolchain; build the source guest and pin its hash; run it in both backends | gate/tool | The generated @0.3.2 component is tracked at `tests/fixtures/wasi/toolchain-guest.wasm`. |
| `setup-curl-wasm` / `test-curl-wasm` | build curl 8.22.0 with wasi-sdk 34; run it against a loopback HTTP server through the CLI with `--net` | tool/test | Real C/WASI network smoke; quiet-mode residual is documented in the script/history. |
| `setup-p2-guests` / `test-p2-guests` | build the wasi-sdk C P2 TCP client and cargo-component P2 HTTP proxy; run both through the CLI | tool/test | Real WASI 0.2 socket and proxy/handler evidence; hashes in `P2-TOOLCHAIN-CHECKSUMS`. |
| `wasi-fixtures` | `nix shell nixpkgs#zig nixpkgs#wasm-tools --command bash tools/build-wasi-fixtures.sh` | utility | Development-only fixture build; the script verifies input hashes and records output hashes in `tests/fixtures/wasi/CHECKSUMS`. The script's own header documents the stricter pinned form, `nix shell --inputs-from . tools/wasi-toolchain.nix --command …`; `tools/build-wasi-fixtures.sh --check` verifies the recorded hashes without building. |
| `report-conformance` | `test-spec-core` | utility | Depends on the core lane and prints where the report went; `core.md` is written by the lane itself. |
| `load-system` | `sbcl --eval '(asdf:load-system :wasmdown)'` | utility | Compile/load check with the repo on `asdf:*central-registry*`. |
| `repl` | `sbcl` with the system loaded | utility | Leaves an interactive SBCL with `wasmdown` loaded. |
| `clean` | — | utility | Declared in `.PHONY`; it currently has no recipe and does nothing. |

Backend selection for the engines is `WASMDOWN_API_MODE=interpret|compiled`
(default `interpret`); the CLI and the WASI runner both read it.

## Generated guests and curl

`tests/toolchain/wasi-guest` is a Rust command guest built with the pinned
rust-toolchain; `tools/build-toolchain-guest.sh` normalises the target std's
`@0.3.0` interface labels to the vendored `@0.3.2` release (same-length,
wasm-tools validated) and pins the artifact hash. The guest runs with exact
stdout in both backends.

`tools/setup-curl-wasm.sh` builds the real curl 8.22.0 as a
`wasm32-wasip3` component with wasi-sdk 34 (`-D_WASI_EMULATED_SIGNAL`, no
TLS/zlib). `make test-curl-wasm` starts a loopback HTTP server and asserts
that `curl -sS -v` retrieves the body through the real CLI with an explicit
`--net 127.0.0.1:PORT` authority. Quiet mode without `-v`/`--trace` can
remain parked after the last body byte in the current async scheduler; the
verbose run is the reproducible gate, and the residual is recorded in
`history.md`.

## Repository layout

```
wasmdown.asd   two systems: wasmdown, and wasmdown/test (the unit suite)
Makefile       the dev entry points above
src/           one file per subsystem, 22 in all: packages, conditions,
               types, numops, simd, binary, binary-encode, component,
               validate, runtime, interp, codegen, api, cli, text, wast,
               component-runtime, component-text, wit, wasi,
               wasi-preview1, wasi-preview2
tests/         *.lisp unit files; spec-core/ (97 pinned .wast), component/
               (vendored suite + nyi.txt), fixtures/wasi/ (pinned guests),
               and generated *-results.jsonl
tools/         lane runners (run-tests, run-spec-core, run-component-suite,
               run-component-census, run-wasi-suite, run-upstream-testsuite),
               wasmdown.lisp (the CLI launcher), fixture builders, probes
specs/         vendored upstream specification text, lock.toml (every pin),
               wasi/ WIT snapshots and inventories — data, not prose
docs/          this documentation set; see docs/README.md for the map
```

Load order matters: `src/packages.lisp` comes first and its package
`:use`/`:import-from` lists are how the subsystem boundaries of
[`design/PLAN.md` §4](design/PLAN.md) are enforced. A new file goes into
`wasmdown.asd`'s `:serial t` component list at the point its dependencies
are already loaded.

## Adding a test

1. Write `tests/<area>.lisp` (or add to an existing one). Each file defines
   its own `wasmdown.test.<area>` package — usually `(:use #:cl
   #:wasmdown.test)`, a few add `#:wasmdown.types` — or works directly in
   `wasmdown.test`; calls are written fully qualified, as
   `wasmdown.test:test`.
2. Register the case with the harness in `tests/harness.lisp`.
   `(test name body…)` pushes a named thunk onto `*tests*` (via
   `eval-when`, so loading the file registers it); `run-all-tests` runs
   them in registration order under a `handler-bind` on `error` — any
   error signalled by the body is a failure. Assertions: `(is form)`,
   `(assert-trap :kind body…)`, `(assert-raises condition-class body…)`,
   with `check-trap` for the non-asserting form. The shape:

   ```commonlisp
   ;; tests/api.lisp — bad magic is a decode error, never a Lisp error
   (wasmdown.test:test api/read-module-decode-error
     (wasmdown.test:assert-raises wasmdown:decode-error
       (wasmdown:read-module #(0 1 2 3 1 0 0 0))))
   ```

   A few files define file-local helpers instead of using the shared
   macros — `tests/numops.lisp` defines its own `is`/`is=` in
   `wasmdown.test.numops`, and `tests/wasi-preview1.lisp` has a `defun`
   `is=`; that is a local convenience, not the harness API.

3. Add the file in **two** places: the `#:wasmdown/test` component list in
   `wasmdown.asd`, and the `dolist` of file names in
   `tools/run-tests.lisp`. `make test-unit` loads through that `dolist`
   after `tests/harness`, which registers `wasmdown.test:run-all-tests`;
   the ASDF system is what makes the files loadable on their own. A file
   missing from the `dolist` shows up in neither count.

Names read `<area>/<what-is-pinned>`, and they are load-bearing: an
existing name is cited by the docs when a fix is verified, so rename
deliberately. Test files may be re-run individually:

```sh
sbcl --non-interactive --eval '(require :asdf)' \
  --eval "(pushnew #P\"$PWD/\" asdf:*central-registry*)" \
  --eval '(asdf:load-system :wasmdown/test)' \
  --eval '(wasmdown.test:run-all-tests)'
```

For a behaviour change that a lane measures, run the lane and read the
diff in its generated report — the counts are the evidence, and a moved
count must be explained in the same change.

## Documentation conventions

| Kind | Files | Rule |
| --- | --- | --- |
| Hand-written | `README.md`, `docs/README.md`, `architecture.md`, `status.md`, `development.md`, `history.md`, `usage/*`, `design/PLAN.md`, `adr/*`, `CONTRACTS.md` | Edit freely, keeping one fact in one home. |
| Generated | [`conformance/core.md`](conformance/core.md), [`component.md`](conformance/component.md), [`wasi.md`](conformance/wasi.md), [`upstream.md`](conformance/upstream.md) | Never edited by hand; fix the runner under `tools/` and regenerate. The command per file is listed in [`docs/README.md`](README.md#generated-vs-hand-written). |
| Data | `specs/`, `tests/component/nyi.txt`, `tests/fixtures/` | Pinned upstream material; left verbatim. |

Numbers live in `conformance/`, scope decisions in `status.md` and
`design/PLAN.md`, behaviour in `usage/`. Anywhere else, link instead of
restating. `design/PLAN.md` is the binding contract: cite it by section
number (`design/PLAN.md §12.7`) from source comments and docs, and do not
renumber its sections — comments in `src/` point at them.

## Dependencies and licence

- **Zero third-party Lisp libraries.** `wasmdown.asd` has an empty
  `:depends-on`, the test harness is self-contained, and no build or test
  step needs Quicklisp. Only two things touch the network: the discovery
  lane, which fetches its mirror, and a deliberate fixture rebuild.
  New dependencies are a design decision, not a convenience:
  [`design/PLAN.md`](design/PLAN.md) and
  [ADR-003](adr/ADR-003-scope-and-phases.md) record the policy.
- **SBCL-specific internals where documented.** Portable ANSI Common Lisp
  for the core logic; the CLI, codegen and memory paths use SBCL
  facilities (`sb-ext`, `sb-alien`, `sb-thread`, `sb-posix`) where the
  design calls for them. Platform assumptions and ceilings are recorded in
  the ADRs, not hidden in the code.
- **Licence: not yet chosen.** `wasmdown.asd` declares `:licence "TBD"`.
  Treat the tree as all-rights-reserved until that changes.
