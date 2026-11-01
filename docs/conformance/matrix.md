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
| 0.3.2 sockets provider (internal composition only) | unit pins incl. `wasi/e2e-tcp-echo-component` PASS | same test binary, host listener + echo thread | `make test-unit`; `tests/wasi.lisp` |
| Pinned 0.3.2 guests (hand-authored) | 6/6 pass ×3 repeats: no-import success smoke, a `wasi:cli/exit@0.3.2` importer (exit 7), a `wasi:clocks/monotonic-clock@0.3.2` importer (monotonic `now()`), a resume-safe async command (callback `task.return`), a `write-via-stream` stdout command (`hi`) and a cancelled-subtask command | 6/6 pass, same cases and repeats; the nested core runs compiled (`backend-entry-count`) | `make test-wasi-preview3`, `tools/build-p3-fixtures.lisp`, `wasi/command-component-runs-the-selected-backend` |

Network communication (TCP, UDP and DNS) is **excluded from the declared
support profile**: the sockets row above is an internal-composition pin for
the pinned `wasi:cli/command` world, not a support row and not a P3 guest
case, and no row here counts a socket case as P3 support. A component-declared
`wasi:sockets` import is refused at link time (see
[`../../specs/wasi/MANIFEST.md`](../../specs/wasi/MANIFEST.md) § “Network
communication: `wasi:sockets` (excluded)”).

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
(SQLite, 2 039 defined functions) needs ~8 GiB of SBCL heap and ~200 s to
compile — about 98 ms per function — which is why WASI compiled runs are
report evidence rather than a gate. The bench table carries each fixture's
function count so compile cost is readable per function.

## Release check

`make verify-release` (`tools/check-release-matrix.py`) reads the recorded
artifacts and requires every row above to be present, green, and to carry
a tested source commit that is HEAD or an ancestor of it (the artifacts are
committed after the run, so a row may record an earlier commit): core parity
summary, component records for both backends, the P2/P3 WASI records, the
bench JSON and the inventory. Each row prints its own source, and the
summary lists every distinct tested commit, so a lagging row (typically the
~37 min compiled P2 lane) is visible rather than averaged away. P1 is
stdout-only and is listed as `n/a` with `make verify` named as its gate.

## Blocked

- **A real-toolchain WASI 0.3.2 guest.** The lane runs a hand-authored
  `@0.3.2` command assembled from the pinned binary grammar
  (`tools/build-p3-fixtures.lisp`, bytes pinned by `P3-CHECKSUMS`); no
  toolchain emits the 0.3.2 component ABI here, so this is ABI coverage, not
  real-toolchain integration evidence.
- **Resume-safe async P3 guest.** The importing guest above is synchronous;
  a stateful async command (continuation state in memory, one-shot side
  effects across reactor wakes) is not pinned yet. The tiny stdout fixture in
  `tests/wasi.lisp` keeps no resume state, so a callback re-invocation
  re-emits its output — a fixture limitation, recorded in `docs/status.md`. The fixture is exercised at the `run-component` level
  and the P3 CLI lane uses the sync command.
  `tools/run-wasi-suite.py --abi preview3` selects that ABI explicitly and
  FAILS on the missing fixture (never skips); `--abi all` covers the two
  released ABIs only.
- **`values/post-return.wast`** is no longer excluded: post-return isolation
  is implemented, the file passes 71/71, and the component lane denominator
  moved from 1,631 to 1,702 commands.
