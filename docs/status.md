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
| Frontend | WIT reader | Partial | World `include`, `implements`/`external-id` and nominal resource typing are not read. |
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
| WASI | Preview 1 (`wasi_snapshot_preview1`) | Full | All 46 pinned imports are provided over an explicit capability context. The whole acceptance lane (in-memory and on-disk SQLite, rollback, bail, preopen denial, read-only, escape) passes with compiled guest cores too. |
| WASI | 0.3.2 providers | Partial | The CLI now owns a reactor (clock deadlines and socket readiness are servable); what remains partial is the provider surface itself — blocking DNS/stdio executors and the `@unstable` gates. |
| WASI | 0.2 facade | Partial | 27 of 31 stable interfaces are bound at exact release names. |
| WASI | 0.3.2 guest lane | Partial | Four hand-authored @0.3.2 commands run through the real CLI in both backends (`make test-wasi-preview3`): a no-import success smoke, a provider-importing guest that lowers `wasi:cli/exit@0.3.2` and exits 7, a clock guest that imports `wasi:clocks/monotonic-clock@0.3.2` and traps unless `now()` is monotonic, and a resume-safe async command whose `run` is lifted async with a callback (`YIELD` from the core run, `task.return(ok)` in the callback, `EXIT`). No upstream toolchain input exists here; socket/stream guests and a provider-driven async wait remain open. |
| WASI | `wasi:http` | Out of scope | Removed from the runtime by decision and not planned, recoverable from git history. |

## Declared limits

These are implementation ceilings, documented rather than hidden. Each is a
dynamic variable or a bounded preflight, never a crash.

| Limit | Value | Where |
| --- | --- | --- |
| Linear memory | 1024 pages / 64 MiB by default; `grow` past it returns the specified -1 | [`ADR-005`](adr/ADR-005-memory-ceiling.md), `wasmdown.runtime:*max-pages*`, `specs/lock.toml` |
| Guest call depth | Traps `:call-stack-exhausted` before the host control stack dies; a tail transfer does not grow it | [`ADR-003`](adr/ADR-003-scope-and-phases.md), `wasmdown.runtime:*guest-depth-limit*` |
| One string literal, or one assembled `(module binary …)` | 64 MiB, a catchable `decode-error` | `src/wast.lisp`, `*wast-max-string-bytes*` |
| Locals per function | 1 000 000, rejected before allocation | `src/wast.lisp`, `*wast-max-locals-per-func*` |
| Compiled backend | Explicit `codegen-error` for any instruction it cannot lower (with function index, byte offset and opcode); no silent fallback | [`usage/errors.md`](usage/errors.md), `src/codegen.lisp` |

A module that needs more than the default ceiling runs by raising
`*max-pages*` together with SBCL's `--dynamic-space-size`; both are required,
and neither is a validation rule (ADR-005).

## Known gaps

- Nested `option<stream<u8>>` passed through a host lower resolves in the
  provider's table instead of the caller's.
- Descriptor destructors and lend lists accumulate in long-lived hosts.
- `@unstable` is parsed and recorded but not enforced; the affected interfaces
  are simply not registered.
- The async-lifted command fixture in `tests/wasi.lisp` is not resume-safe.
  Under the canonical async ABI a resumed task re-enters its callback and a
  stateful guest resumes from the state it kept in memory; this
  four-instruction fixture keeps none and writes unconditionally, so each
  re-entry emits `hi` again (stdout repeats, then the deadlock oracle).
  That is a property of the fixture, not of the scheduler: it is green
  through `run-component` without a reactor, and the P3 CLI lane uses the
  sync command until a resume-safe async guest is pinned.
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
  stream does not yet transition to `closed`.
- Preview 1 `proc_raise` returns `NOTSUP`; a guest cannot signal itself.
- No **upstream-toolchain** WASI 0.3.2 guest is pinned (no toolchain here
  emits the 0.3.2 component ABI). The lane runs a hand-authored command
  generated by `tools/build-p3-fixtures.lisp` and pinned by
  `P3-CHECKSUMS` (the binary itself is untracked and regenerated);
  `--abi preview3` is explicit-only and `--abi all` covers the two released
  ABIs. Provider-level 0.3.2 coverage is measured by the component lane and
  unit pins, not by that guest.

## Roadmap

**Providers.** The CLI owns its reactor now (socket readiness watches and
`clocks.wait-until`/`wait-for` work without an embedder); what remains is
keeping every blocking host path (DNS, blocked stdio) on the bounded
executors rather than a caller's thread.

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
