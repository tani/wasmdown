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
| Backends | Baseline codegen | Partial | No `return_call`/`return_call_indirect`, so tail-recursive fixtures recurse on the host stack. |
| Backends | Optimized or AOT tier | Not implemented | The engine ships an interpreter and a baseline compiler only. |
| Backends | FASL cache | Not implemented | Compiled functions are not persisted across processes. |
| Backends | Threads and atomics | Not implemented | Shared memory and the atomic instruction family are absent. |
| Backends | Annotations proposal | Out of scope | Excluded permanently by the deliberate-exclusion list in [`design/PLAN.md` §3.3](design/PLAN.md). |
| Component model | Binary and text decoding | Full | Both front ends build one component IR, including WIT value types and aliases. |
| Component model | Instantiation and linking | Full | Two-level core-instance linking, component `with`-args and alias projection execute. |
| Component model | Canonical ABI | Full | Lift/lower, flattening, string transcoding, `map` despecialization and post-return run at runtime. |
| Component model | Resources | Full | Per-instance handle tables, own/borrow transfer, destructors and method dispatch. |
| Component model | Async execution | Partial | A live borrow held across `task.return` does not yet trap. |
| WASI | Preview 1 (`wasi_snapshot_preview1`) | Full | All 46 pinned imports are provided over an explicit capability context. |
| WASI | 0.3.2 providers | Partial | The default CLI context carries no reactor, so clock waits are unavailable and sockets poll once. |
| WASI | 0.2 facade | Partial | 27 of 31 stable interfaces are bound at exact release names. |
| WASI | `wasi:http` | Out of scope | Removed from the runtime by decision and not planned, recoverable from git history. |

## Declared limits

These are implementation ceilings, documented rather than hidden. Each is a
dynamic variable or a bounded preflight, never a crash.

| Limit | Value | Where |
| --- | --- | --- |
| Linear memory | 1024 pages / 64 MiB by default; `grow` past it returns the specified -1 | [`ADR-005`](adr/ADR-005-memory-ceiling.md), `wasmdown.runtime:*max-pages*`, `specs/lock.toml` |
| Guest call depth | Traps `:call-stack-exhausted` before the host control stack dies | [`ADR-003`](adr/ADR-003-scope-and-phases.md), `wasmdown.interp:*call-depth-limit*` |
| One string literal, or one assembled `(module binary …)` | 64 MiB, a catchable `decode-error` | `src/wast.lisp`, `*wast-max-string-bytes*` |
| Locals per function | 1 000 000, rejected before allocation | `src/wast.lisp`, `*wast-max-locals-per-func*` |
| Compiled backend | No `return_call*`; 11 of the 97 pinned core files diverge under `--backend baseline` | [`conformance/core.md`](conformance/core.md), `src/codegen.lisp` |

A module that needs more than the default ceiling runs by raising
`*max-pages*` together with SBCL's `--dynamic-space-size`; both are required,
and neither is a validation rule (ADR-005).

## Known gaps

- No reactor is attached by default: the CLI builds a `wasi-context` without
  `:reactor`, so `clocks.wait-until`/`wait-for` return `host-error` and a socket
  source falls back to one bounded readiness poll instead of parking on a watch.
- Nested `option<stream<u8>>` passed through a host lower resolves in the
  provider's table instead of the caller's.
- Descriptor destructors and lend lists accumulate in long-lived hosts.
- `%abi-lower` does not handle the top-level discriminant of error results with
  compound outer shapes.
- `@unstable` is parsed and recorded but not enforced; the affected interfaces
  are simply not registered.
- WIT world `include` and `implements`/`external-id` forms are not read by the
  frontend.
- Preview 1 `proc_raise` returns `NOTSUP`; a guest cannot signal itself.

## Roadmap

**Providers.** Attach a `wasi-reactor` to the CLI's context so socket
readiness watches and `clocks.wait-until`/`wait-for` work without an embedder
supplying one, and keep every blocking host path (DNS, blocked stdio) on the
bounded executors rather than a caller's thread.

**Canonical ABI.** Revisit the `borrow` lend arms that survive in the two
canon-lower wrappers; the position-dependent `stream`/`future` arms are already
gone. Add the borrow-side lifetime pins — a borrow held live across
`task.return` must trap — reusing the shape of the guest-drop pin.

**Backends.** Build the parity matrix between interpreter and baseline, add
differential fuzz and property lanes, and wire the remaining CI lanes around
them. The baseline tier is correctness-first; unboxing i64 and removing the
extra `funcall` on compiled-to-guest edges are performance work, not scope.

**WASI.** Finish the 0.1 and 0.2 facades; the 0.2 registry already binds 27 of
31 stable interfaces at exact release names.

The engineering record — how each of these arrived, and the measurements that
drove the decisions — is in [`history.md`](history.md); the lane measurements
quoted here are regenerated by the lanes documented in
[`conformance/`](conformance/README.md).
