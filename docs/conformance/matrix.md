# Backend × ABI matrix

Two guest backends are selectable and explicit — `:interpret` (the reference)
and `:compiled` (every defined function compiled before the start section) —
and the same host providers serve both. This page records, per surface, what is
measured and where the numbers live. It is hand-written; the lane artifacts it
links are generated.

## Measured matrix

| Surface | `:interpret` | `:compiled` | Evidence |
| --- | --- | --- | --- |
| Pinned core suite (97 scripts, 21 260 commands) | 21 187 pass / 0 fail / 73 unsupported | 21 260 / 21 260 commands agree in outcome and phase; 0 compile refusals | [`core.md`](core.md), [`backends.md`](backends.md), `make test-backend-parity` |
| Vendored component suite (1 702 commands) | 1 702 / 0 / 0 / 0 — released gate | 1 702 / 0 / 0 / 0 — same denominator | [`component.md`](component.md), `tests/component-compiled/component.md` (`make report-component-compiled`) |
| WASI Preview 1 acceptance (in-memory and on-disk SQLite) | 14/14 checks pass | 14/14 checks pass: smoke, persistence (two processes), reopen, rollback, bail, no-preopen, read-only, escape — 821 s of compile-heavy wall time. The lane asserts exact stdout/status per case, so both backends meet identical expectations | `tools/wasi-preview1-acceptance.py [--backend compiled]` |
| WASI 0.2 behaviour lane (11 cases) | 11/11 pass | 11/11 pass (11 x ~200 s compile wall time). Each case's manifest entry asserts exact stdout bytes and exact status, so both backends satisfy the SAME byte-level expectations — stronger than a post-hoc sha diff. Per-case records live under `tests/wasi-results/` (`latest/` for interpret, the compiled run's `--output-dir`), distinguished by the `backend` field | `tools/run-wasi-suite.py --abi preview2 [--backend compiled]` |
| WASI 0.3.2 providers | unit/provider pins, component async suite | same providers; component suite green under `:compiled` | [`inventory.md`](inventory.md), `tests/wasi*.lisp` |
| Pinned 0.3.2 guest (hand-authored) | 1/1 pass (command world, exit 0) | 1/1 pass; the nested core runs compiled (`backend-entry-count`) | `make test-wasi-preview3`, `tools/build-p3-fixtures.lisp`, `wasi/command-component-runs-the-selected-backend` |

The backend property lives on the function, not on a replacement object: the
selected mode compiles each defined function's existing `func-instance`
(`backend`, `compiled-entry`), so exports, tables, element segments, `ref.func`
and other modules' imports keep the same reference, and an imported function
keeps the backend of the instance that defined it. A compile refusal is
`wasmdown:codegen-error` (function, byte offset, opcode) raised before active
segments and before the start section; there is no interpreter fallback.

## Resource behaviour

- Every host call re-resolves the guest memory, so `memory.grow` between two
  imports is visible to both backends (`tests/wasi-preview1.lisp`
  `memory-grow-between-host-calls`, run in both modes).
- Descriptor release is asserted on success, trap and `proc_exit` paths, and a
  rejected `path_open` out-pointer releases the fd it had opened; the P2 audit
  pins the accept/stream reference-count arithmetic (`tcp-accept-does-not-leak-descriptors`).
- Component handles reuse their free-list slot; the async suite's
  `drop-cross-task-borrow` and `passing-resources` fixtures exercise
  cross-task borrow accounting in both backends.

## Cost

[`performance.md`](performance.md) (`make report-backends-bench`) separates
decode / validate / instantiate(+compile) / run per backend. On the synthetic
kernels `:compiled` runs 7–17x faster than the interpreter, and eager
compilation costs 0.4–1.5 ms and 0.3–1.3 MiB per small module. A 6 MB guest
(SQLite) needs ~8 GiB of SBCL heap and ~200 s to compile, which is why WASI
compiled runs are report evidence rather than a gate.

## Blocked

- **A real-toolchain WASI 0.3.2 guest.** The lane runs a hand-authored
  `@0.3.2` command assembled from the pinned binary grammar
  (`tools/build-p3-fixtures.lisp`, bytes pinned by `P3-CHECKSUMS`); no
  toolchain emits the 0.3.2 component ABI here, so this is ABI coverage, not
  real-toolchain integration evidence.
- **Async-lifted command through the CLI.** The tiny stdout fixture in
  `tests/wasi.lisp` keeps no resume state (its body writes unconditionally),
  so a callback re-invocation after a reactor wake re-emits its output; that
  is a fixture limitation, recorded in `docs/status.md`. The fixture is exercised at the `run-component` level
  and the P3 CLI lane uses the sync command.
  `tools/run-wasi-suite.py --abi preview3` selects that ABI explicitly and
  FAILS on the missing fixture (never skips); `--abi all` covers the two
  released ABIs only.
- **`values/post-return.wast`** is no longer excluded: post-return isolation
  is implemented, the file passes 71/71, and the component lane denominator
  moved from 1,631 to 1,702 commands.
