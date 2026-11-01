# Status

The runtime declares `declared_support = partial` in `specs/lock.toml`. This
page is the per-area reading of that word: what is complete, what is
deliberately incomplete, and what will not be built. Measurements live in
[`conformance/`](conformance/README.md); scope decisions live in
[`design/PLAN.md`](design/PLAN.md) and the [`adr/`](adr/README.md).

## Support matrix

| Area | Capability | State | Basis |
| --- | --- | --- | --- |
| Frontend | Core binary decoder | Full | Every section and opcode in the declared scope decodes with bounds checks and byte offsets. |
| Frontend | Binary encoder | Full | Canonical minimal output round-trips through the decoder. |
| Frontend | Core text format (`.wat`) | Full | Produces the binary decoder's IR for the whole implemented opcode set. |
| Frontend | Spec scripts (`.wast`) | Full | Commands, assertions and failure-phase classification run against shared script state. |
| Frontend | Component text | Full | Parses the pinned validation suite's grammar into the component IR. |
| Frontend | WIT reader | Full (pinned grammar) | World `include`/renaming, named `implements` and positional `@external-id` bindings, and nominal resource identity are parsed, resolved and pinned by tests (`tests/wit.lisp`: include-of-includes, rename equivalence, resource nominal identity across interfaces). Residual validator limits are tracked per case, not as a blanket WIT gap. |
| Validation | Core validation | Full | Operand/control-stack algorithm with unreachable polymorphism over the declared scope. |
| Validation | Component validation | Full | Label, index, canon, type-identity and instantiation-matching rules pass the pinned suite. |
| Validation | Component type-use limits | Partial | Compound outer refs in imported-instance functypes are rejected by the text/validator front end. |
| Validation | Hostile-input preflight | Full | Oversized literals and local counts are rejected as catchable `decode-error`s before allocation. |
| Backends | Interpreter | Full | Default backend: scalar, SIMD, GC, EH, tail calls, multi-memory and memory64 semantics. |
| Backends | Baseline codegen | Full for opcode coverage | Compiles every defined function before the start section and lowers every instruction the interpreter implements (scalar, memory, table, control, references, EH, tail calls, SIMD incl. relaxed, GC). An instruction with no lowering is a `codegen-error`, never an interpreter fallback. |
| Backends | Backend parity (pinned core) | Full | `tools/run-backend-parity.lisp`: 21,260 / 21,260 commands agree in outcome and phase across both explicitly selected backends. |
| Backends | Optimized or AOT tier | Not implemented | The engine ships an interpreter and a compiled backend only. |
| Backends | FASL cache | Not implemented | Compiled functions are not persisted across processes. |
| Backends | Threads and atomics | Not implemented | Shared memory and the atomic instruction family are absent. |
| Backends | Annotations proposal | Out of scope | Excluded permanently by the deliberate-exclusion list in [`design/PLAN.md` §3.3](design/PLAN.md). |
| Component model | Binary and text decoding | Full | Both front ends build one component IR, including WIT value types and aliases. |
| Component model | Instantiation and linking | Full | Two-level core-instance linking, component `with`-args and alias projection execute. |
| Component model | Canonical ABI | Full | Lift/lower, flattening, string transcoding, `map` despecialization and post-return run at runtime. |
| Component model | Resources | Full | Per-instance handle tables, own/borrow transfer, destructors and method dispatch. |
| Component model | Async execution | Partial | The task/subtask state machine, cancellations and resource lifetimes run the whole async suite in both backends. Live-borrow accounting traps `task.return`/`task.cancel` with outstanding borrows; residual ABI/lifetime gaps are listed under Known gaps. |
| Component model | Backend parity | Full | The whole component lane (1,702 commands) is green with every defined core compiled before its start section (`make report-component-compiled`). |
| WASI | Preview 1 (`wasi_snapshot_preview1`) | Partial (network imports excluded) | All 46 pinned imports are provided over an explicit capability context; the whole acceptance lane (in-memory and on-disk SQLite, rollback, bail, preopen denial, read-only, escape) passes with compiled guest cores too. The four legacy `sock_*` imports remain implemented and legacy-tested, but network communication is excluded from the declared support profile (2026-11-02; see the `wasi:sockets@0.3.2` row), so the P1 claim is explicitly the NON-NETWORK P1 surface; no network behaviour is claimed. |
| WASI | 0.3.2 providers | Partial | The CLI owns a reactor (clock deadlines and stream readiness are servable); what remains partial is the provider surface itself — the blocked-stdio executor audit and the canonical-ABI/lifetime residuals listed under Known gaps. Declared `@unstable` imports are refused at link time (see Known gaps), so stability gating is no longer a missing piece. `wasi:sockets` is excluded, not partial; see the `wasi:sockets@0.3.2` row. |
| WASI | 0.2 facade | Partial | 27 of 31 stable interfaces are bound at exact release names. The bound `wasi:sockets/*` interfaces are legacy-tested through the embedding API only; the CLI refuses `--net/--listen/--dns` for every run, so no P2 sockets path exists through the CLI. Network communication is excluded from the declared support profile (see the `wasi:sockets@0.3.2` row) and no P2 network behaviour is claimed. |
| WASI | `wasi:http` | Unsupported by design (not a gap, not planned) | Deliberate, permanent exclusion by user decision (2026-11-02); removed in M17 and recoverable from git history. The vendored `specs/wasi/wasi-wit-0.3.2/http/*.wit` is reference data only. An import is refused with `wasi:http is not supported by this runtime by design (removed in M17); the pinned HTTP WIT is reference data only. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.` See [`specs/wasi/MANIFEST.md`](../specs/wasi/MANIFEST.md) § “Unsupported by design (not pending)”. Not counted as an incomplete interface. |
| WASI | `wasi:sockets@0.3.2` | Unsupported by design (not a gap, not planned) | TCP, UDP and DNS are excluded permanently by user decision (2026-11-02); there is no `wasi:sockets` provider for guests. The vendored `specs/wasi/wasi-wit-0.3.2/sockets/sockets.wit` is reference data only. A **0.3-line** component-declared import (`types`, `ip-name-lookup`, any 0.3.x version) is refused at link time with `Network communication (TCP, UDP and DNS) is not supported by this design; there is no wasi:sockets provider for guests. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.` The pinned `wasi:cli/command` world imports sockets itself, so a 0.3.2 sockets provider is retained internally for that composition only. See [`specs/wasi/MANIFEST.md`](../specs/wasi/MANIFEST.md) § “Network communication: `wasi:sockets` (excluded)”. Not counted as an incomplete interface. |
| WASI | 0.3.2 guest lane | Partial | Seven hand-authored @0.3.2 commands run through the real CLI in both backends (`make test-wasi-preview3`, 3 repeats, 21/21 per backend): a no-import smoke; a `wasi:cli/exit@0.3.2` importer (exit 7); a `wasi:clocks/monotonic-clock@0.3.2` importer; a resume-safe async callback command; a `write-via-stream` stdout command printing `hi`; a cancelled-subtask command; and a `wasi:random/random@0.3.2` guest with `insecure`/`insecure-seed` (two 16-byte reads must have exact lengths and differ; `get-random-u64`/seed must complete), lowering the list returns through memory+realloc. The stream and random guests are authored in TEXT and encoded by `tools/component-encode.lisp`, guarded by `wasi/component-text-encoder-roundtrip`. The lane stays Partial because no upstream @0.3.2 toolchain guest exists here; provider behaviour is measured by this lane and the component tests. The former TCP loopback socket command was removed with the network exclusion (2026-11-02); network is not part of this lane. |

## Known gaps and accepted limits

- Nested `option<stream<u8>>` passed through a host lower resolves in the
  provider's table instead of the caller's.
- Descriptor destructors and lend lists accumulate in long-lived hosts.
- `@unstable` imports are **enforced at link time**. The 0.3 composition path
  checks every declared `wasi:` import through the WIT selection gate
  (`wit-import-surface-status`) before anything is composed: if the target
  interface is `@unstable(feature = …)` and that feature is not enabled by the
  selected stable surface, the run is refused with a `link-error` naming the
  import and the feature, for example:

  ```
  wasi:clocks/timezone@0.3.2 is @unstable(feature = clocks-timezone) and that feature is not enabled; unstable interfaces are outside the selected stable WASI 0.3.2 surface (see docs/usage/wasi.md and specs/wasi/MANIFEST.md).
  ```

  Stable imports are unaffected. **No CLI flag enables a feature today**; the
  gate is refusal-only. Pinned by
  `wasi/component-unstable-declared-import-is-link-error`,
  `wit/selection-import-surface-query` and
  `wit/selection-command-world-timezone-import`.
- The four-instruction async fixture in `tests/wasi.lisp`
  (`wasi/e2e-command-component-binary`) is not resume-safe: it writes
  unconditionally, so under a reactor-driven resume each re-entry emits `hi`
  again. That is a property of that fixture, not of the scheduler. It is
  retained as the in-process `run-component` case; the CLI lane now pins
  resume-safe commands instead (`p3/async-callback` keeps its state in
  memory; `p3/stream-stdout` parks the future read, stages the write and
  waits on real events), so the lane no longer depends on the
  four-instruction body.
- The P3 `write-via-stream` path is now exercised end to end by a pinned
  guest (`p3/stream-stdout`, 3 repeats, both backends): it prints exactly
  `hi\n` through the worker-backed CLI stdout. Two runtime defects found
  while building it are fixed and pinned: `%host-end-retry` cleared the
  parked host copy before its retry (so every readiness resubmitted the
  span and stdout duplicated — now the copy is reused), and `%end-notify`
  woke only a sync waiter, never a `waitable-set` waiter (so a task parked
  on the set never learned a stream/future copy completed — now both are
  woken). The guest itself must use the canonical order: park the future
  read first, stage the write, wait for a WOULD-BLOCK write's own event
  (3) before `drop-writable`, then wait for the future event (4).

- WASI 0.2 (shared by both backends, after the readiness/permit audit):
  end-of-input is still reported as `stream-error::closed` rather than
  `ok([])` (the open-but-empty vs closed distinction itself is now exact:
  `read(0)` and an open source with no bytes return `ok([])`, while a
  closed rep returns `closed`); `check-write`/`check-send` permits are
  enforced (a write or datagram batch beyond the offer traps) and an
  unwritten write suffix is retained for the next flush; permit values are
  host-chosen rather than queried from the socket buffer;
  `sockets/network-error-code` has helper-level coverage only (it is
  `@unstable` at 0.2.12 and not exported); a `last-operation-failed`
  stream does not yet transition to `closed`. The socket-specific items in
  this bullet describe the excluded legacy network path only; they are not
  part of the declared support profile.
- Preview 1 `proc_raise` returns `NOTSUP`; a guest cannot signal itself.
- Compiled-backend compile time/heap is an ACCEPTED limit (owner decision,
  2026-11-02): no reduction ships. Measurements and the rejected levers are in
  `docs/conformance/performance.md`; the untested control-graph lever is fenced
  by the M3 TAGBODY/GO contract.

- P3 async **cancellation** is exercised end to end by `p3/cancel-subtask`
  (3 repeats, both backends). Getting there fixed a real defect:
  `%canon-lower-async-host-func` packed its STARTED/RETURNED state under the
  callee task's mutex, but a HOST-PENDING callee deliberately has no
  `async-task` record, so the pack dereferenced NIL (TYPE-ERROR inside
  `%CANON-LIFT-ASYNC`). The host-pending arm now packs without the mutex
  (its resolution is delivered later by the pump, so there is no second
  reader to race). Encoding note: lowering an async function needs the
  `async` canon option — it flattens to `(i64)->i32`; without it the lower
  flattens to `(i64)->()` and fails the core-import signature check.
- No **upstream-toolchain** WASI 0.3.2 guest is pinned (no toolchain here
  emits the 0.3.2 component ABI). The lane runs hand-authored commands
  generated by `tools/build-p3-fixtures.lisp` and pinned by
  `P3-CHECKSUMS` (the binary itself is untracked and regenerated);
  `--abi preview3` is explicit-only and `--abi all` covers the two released
  ABIs. Provider-level 0.3.2 coverage is measured by the component lane and
  unit pins, not by that lane.

## Roadmap

**Providers.** The CLI owns its reactor now (clock deadlines and stream
readiness work without an embedder); what remains is keeping every blocking
host path (blocked stdio) on the bounded executors rather than a caller's
thread. Network paths are excluded and not part of this roadmap.

**Canonical ABI.** Revisit the `borrow` lend arms that survive in the two
canon-lower wrappers; the position-dependent `stream`/`future` arms are already
gone. Live-borrow accounting already traps `task.return`/`task.cancel`, and the
compound `result` layouts are pinned; the open items are the nested
`option<stream<u8>>` owner resolution and lend/destructor reclamation.

**Backends.** The pinned core parity lane, the component compiled report and
the cost bench exist; what remains is differential fuzz/property lanes and the
remaining CI wiring (see [`conformance/matrix.md`](conformance/matrix.md)).
The compiled tier is correctness-first; unboxing i64 and removing the extra
`funcall` on compiled-to-guest edges are performance work, not scope.

**WASI.** Finish the 0.1 and 0.2 facades; the 0.2 registry already binds 27 of
31 stable interfaces at exact release names.

The engineering record — how each of these arrived, and the measurements that
drove the decisions — is in [`history.md`](history.md); the lane measurements
quoted here are regenerated by the lanes documented in
[`conformance/`](conformance/README.md).
