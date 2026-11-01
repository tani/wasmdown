# Conformance

A **lane** is one test corpus plus the runner that measures it. Each lane has
a fixed command; the reporting lanes below add a pinned upstream revision and
a committed artifact. Lanes are independent: their counts are never added
together, and a number from one lane says nothing about another.

## Lanes

| Lane | Kind | Command | Artifact | What it proves |
| --- | --- | --- | --- | --- |
| unit | gate | `make test-unit` | stdout | Every unit and integration test passes; exits non-zero on any failure. |
| spec-core | baseline | `make test-spec-core` | [`core.md`](core.md), `tests/spec-core-results.jsonl` | The pinned core spec suite executes end to end through the stateful WAST runner, with every non-pass outcome classified. |
| component | gate | `make test-component` | [`component.md`](component.md), `tests/component-results.jsonl` | The vendored component suite runs with no fail, skip, timeout, or harness error, and its census is intact. |
| component-baseline | report | `make report-component-baseline` | `tests/component-baseline/component.md`, `tests/component-baseline-results.jsonl` | The same component corpus with every defined core function compiled before its start section; a compile refusal is data here, and the released gate is the interpret run. |
| component-census | gate | `make test-component-census` | `tmp-census/run-N.log`, rewrites [`component.md`](component.md) | The component gate stays green over repeated fresh-process runs at the pinned denominator. |
| backend-parity | gate / report | `make test-backend-parity` (gate), `make report-backend-parity` (report) | [`backends.md`](backends.md), `tests/backend-parity-results.jsonl`, `tests/backend-parity-summary.json` | The same pinned core scripts run through `:interpret` and `:baseline`; every command pairs with equal outcome and phase, with no baseline compile refusal and no command-count drift. A gate run writes only to `tests/backend-parity/`, so it can never clobber the committed report/summary. |
| inventory | check | `make test-backend-inventory` (regenerate: `make report-backend-inventory`) | [`inventory.md`](inventory.md), `tests/backend-inventory.json` | The pinned WASI census (P1 functions, P2/P3 interfaces) reconciles with what the runtime actually registers; excluded and not-implemented surfaces are separate rows. |
| wasi preview1 | gate | `make test-wasi-preview1` (interpret); `tools/wasi-preview1-acceptance.py --backend baseline` (report) | stdout | The real CLI runs pinned SQLite guests with exact stdout/status, plus preopen, read-only, and escape denials. The baseline form runs the same cases with every guest core compiled (~200 s per fresh process); its exact stdout/status must match. |
| wasi preview2 | gate | `make test-wasi-preview2` | `tests/wasi-results/latest/` | WASI 0.2 behaviour with one fresh guest process per case. |
| wasi preview3 | gate (explicit) | `make test-wasi-preview3` | `tests/wasi-results/latest/` | The pinned hand-authored @0.3.2 command (`tools/build-p3-fixtures.lisp`) runs through the real CLI in both backends. Not part of `--abi all`; a missing fixture is a failure, never a skip. |
| wasi all | gate | `make test-wasi` | [`wasi.md`](wasi.md), `tests/wasi-results/latest/` | Both released ABIs (`preview1`, `preview2`) in one run. |
| upstream | discovery | `make test-upstream` | [`upstream.md`](upstream.md), `tests/upstream-results.jsonl` | Where the engine stands against proposal corpora beyond its declared scope. Never gated. |

Pins:

| Suite | Revision |
| --- | --- |
| WebAssembly/spec (core) | `b0ec1f3f449525c217dbff3f1dfab435a756ea92` |
| WebAssembly/component-model | `d095e2f60ddae1f9a1e9b1e2fb547ec1114da677` |
| WASI WIT | v0.3.2 |

## Honest counting

Every command gets exactly one outcome: `pass`, `fail`, `skip`, `unsupported`,
`timeout`, or `harness-error`. An `unsupported` command is a suite feature the
engine classifies as out of scope — it is reported as itself and is never
rewritten as a pass. `skip` and `timeout` carry the same rule. This is why the
denominator is "commands executed" rather than "commands that should pass".

The three kinds differ in what they do with those outcomes:

- A **gate** exits non-zero on any outcome that is not `pass`. If a gate run is
  green, every command in its corpus passed.
- The **census** is a gate over gates: a shrinking denominator, a planned file
  that contributes no commands, or a duplicate command id fails the run.
- A **baseline** is read, not gated. It exits zero, prints the counts, and is
  compared by eye against the previous baseline; a moved count must be
  explained in the same change that moves it.
- **Discovery** reports the same outcomes for corpora the engine does not
  claim, so a failure there is information, not a build break.

Because a gate's denominator is the number of executed commands, its counts
only mean something alongside the census: the gated lanes pin their
denominator, so a corpus that silently stops contributing is caught even when
nothing fails.

## Regenerating

| Report | Command |
| --- | --- |
| [`core.md`](core.md) | `make test-spec-core` |
| [`component.md`](component.md) | `make test-component` (or `make test-component-census` for N repeats) |
| [`backends.md`](backends.md) | `make report-backend-parity` |
| [`inventory.md`](inventory.md) | `make report-backend-inventory` |
| [`performance.md`](performance.md) | `make report-backends-bench` |
| [`wasi.md`](wasi.md) | `python3 tools/run-wasi-suite.py --abi all --repeat 30 --commit-summary` |
| [`upstream.md`](upstream.md) | `make test-upstream` |

These files are generated; edit the runner under `tools/`, never the
report. `make report-backends-bench` also writes
[`performance.md`](performance.md) (per-backend decode / validate /
instantiate(+compile) / run medians) and `tests/bench-backends.json`. See [`../development.md`](../development.md) for prerequisites and
[`../status.md`](../status.md) for what the engine claims to support.

## Reports

[`matrix.md`](matrix.md) is the hand-written backend x ABI view: what each
surface measures under `:interpret` and `:baseline`, plus the blocked items.

- [`core.md`](core.md) — core spec-script baseline.
- [`component.md`](component.md) — component lane gate, with census.
- [`backends.md`](backends.md) — interpret/baseline parity over the pinned core scripts.
- [`wasi.md`](wasi.md) — WASI behaviour lane.
- [`upstream.md`](upstream.md) — aggregated upstream testsuite mirror.
