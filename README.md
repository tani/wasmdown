# WASMDOWN

A Common Lisp–native WebAssembly runtime, implemented per `PLAN.md`.

**Status: `partial` (per PLAN.md §2 support-declaration policy).**

This repository contains a working vertical slice of the plan: binary
decoding, the `.wat` text format, validation, a reference interpreter,
baseline SBCL code generation, a public Lisp API and CLI for the **core
Wasm subset** below, plus the stateful `.wast` spec-script runner and a
measured baseline against the vendored official test suite. It is
**not** a conformant Core 3.0 implementation and makes no conformance
claim. No Component Model, WIT, WASI, SIMD, GC, exception-handling or
threads support yet — those are PLAN.md milestones M5, M7–M12.

## Conformance baseline (honest, per PLAN.md §12.8)

The official core suite (97 `.wast` files, pinned at
`b0ec1f3f449525c217dbff3f1dfab435a756ea92` in `specs/lock.toml`)
executed through the stateful runner with failure-phase classification:

```
pass 19635 | fail 661 | unsupported 746 | harness-error 145 | total 21187
pass rate 92.7% (denominator = all executed, incl. unsupported)
```

Regenerate with `make report-conformance` (writes `CONFORMANCE.md` +
`tests/spec-core-results.jsonl`, one JSONL line per command); the full
97-file run completes in ~3 seconds and must stay fast — a slow or
hanging run is a bug, not a workload.
The pass-rate DROP from the prior 95.2% baseline is deliberate
and honest (PLAN.md §12.8): the `(module quote …)` wrap now parses
~543 payloads that previously "passed" without ever being parsed —
misattributed passes may not be recorded as pass. The strictness
fixes already clear whole families (float_literals 179/179,
int_literals 51/51); the remaining converted cases are honest
fails/harness-errors under active triage. This is a
baseline measurement, **not** a release gate; remaining failures are
case-specific engine gaps plus out-of-scope features (SIMD/GC/EH/tail
calls/typed funcrefs/memory64), classified `unsupported` and never
silently passed. The `unsupported` rise from earlier baselines
(277→725) is honest reclassification: actions against genuinely
out-of-scope modules now classify cleanly instead of crashing as
harness-errors.

### Known gaps (tracked, not hidden)

- **Script-syntax extensions**: `annotations.wast` (`@…` forms) and
  `inline-module.wast` (bare-field scripts) are harness-errors — spec
  test extensions outside the core grammar this runner implements.
- **Implementation ceiling**: default linear-memory limit 1024 pages
  (64 MiB, ADR-005); suite cases above it fail honestly. Raise
  `wasmdown.runtime:*max-pages*` + `--dynamic-space-size` for more.
- **Hostile-input bounds**: 64 MiB string literal / assembled-binary
  ceiling and a 1M locals-per-function validator preflight
  (`src/wast.lisp`) — both catchable `decode-error`s, never crashes.
- **Runner cosmetic**: a failed module command leaves the script's
  current-instance unset; subsequent `invoke`/`get` fail (classified
  `fail`), matching the spec harness's observable behavior.

## Implemented surface

| Subsystem | File | State |
| --- | --- | --- |
| Binary decoder | `src/binary.lisp` | MVP + bulk-memory + sign-ext + non-trapping sat conversions + multi-value + reference types. Bounds-checked reader; padded LEB128 per spec; failure phase = `decode-error` with byte offset. |
| Binary encoder | `src/binary-encode.lisp` | Canonical minimal encodings; round-trip tested. |
| Numeric semantics | `src/numops.lisp` | Exact i32/i64/f32/f64 primitive semantics: traps, NaN/±0 rules, saturating + trapping conversions, reinterpret, little-endian memory helpers. |
| Validation | `src/validate.lisp` | Operand/control-stack algorithm with unreachable-polymorphism; module-level invariants; const-expr rules. |
| Runtime | `src/runtime.lisp` | Module instantiation, memories (64 KiB pages), tables, globals, import matching, elem/data segments, start functions. |
| Interpreter | `src/interp.lisp` | Reference evaluator; guest recursion traps `:call-stack-exhausted` before host stack exhaustion. |
| Codegen | `src/codegen.lisp` | Baseline backend: SBCL `COMPILE` of emitted `LET`/`TAGBODY`/`GO` forms; interp-vs-compiled parity tests. |
| API / CLI | `src/api.lisp`, `src/cli.lisp` | `read-module`, `validate-module`, `instantiate`, `invoke`, …; CLI `validate`/`inspect`/`compile`/`run`. |

Not implemented (see `PLAN.md` for the full contract): SIMD (v128),
Wasm GC, exception handling (`try_table`), tail calls, memory64,
multi-memory runtime (decode accepts memidx forms), Component Model,
Canonical ABI, WASI (all versions), threads/atomics, FASL cache.
The `.wat` text format and `.wast` spec runner **are** implemented
(`src/text.lisp`, `src/wast.lisp`), as is conformance reporting.

## Error/phase taxonomy (PLAN.md §12.2)

`decode-error` (malformed binary; byte offset) → `validation-error`
(bad typing) → `link-error` (import mismatch) → `instantiation-error`
(segment bounds etc.) → `wasm-trap` (runtime; kinds include
`:unreachable :out-of-bounds-memory :out-of-bounds-table
:integer-divide-by-zero :integer-overflow :invalid-conversion
:null-reference :indirect-call-type-mismatch :call-stack-exhausted`).
These classes are never interchangeable.

## Usage

```sh
make test                 # full unit/integration suite (139+ tests)
sbcl --script tools/wasmdown.lisp validate file.wasm
sbcl --script tools/wasmdown.lisp inspect file.wasm
sbcl --script tools/wasmdown.lisp run file.wasm --invoke add 42 13
```

```commonlisp
(let* ((module (wasmdown:read-module #P"example.wasm"))
       (checked (wasmdown:validate-module module))
       (instance (wasmdown:instantiate checked)))
  (wasmdown:invoke instance "add" 42 13))
```

## Repository layout

Mirrors PLAN.md §11.3 (subset): `src/` (one file per subsystem),
`tests/`, `tools/` (test runner + CLI launcher), `docs/CONTRACTS.md`
(binding cross-slice contracts), `docs/adr/` (decision records),
`Makefile`, `wasmdown.asd`.

## Dependencies

SBCL (developed against 2.6.4, Linux ARM64). Zero third-party Lisp
libraries; no ASDF ecosystem dependencies. ANSI-portable core logic with
SBCL-specific numeric internals where documented.
