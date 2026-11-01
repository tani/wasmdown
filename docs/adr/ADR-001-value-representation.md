# ADR-001: Value representation

Date: 2026-10-23 · Status: accepted

## Context

docs/design/PLAN.md §7 requires exact i32/i64/f32/f64 semantics and flags i64 as the
risky case (CL fixnums cannot hold all 64-bit patterns).

## Decision

- i32 and i64 values are **unsigned Lisp integers** `0 .. 2^N-1` at
  every runtime boundary (operand stack, locals, globals, memory, host
  functions). Signed views exist only inside `wasmdown.numops`
  (`i32-s`/`i64-s`) and are applied per-opcode.
- f32/f64 are CL `single-float`/`double-float`; bit-exact reinterpret
  goes through `f32-bits`/`f64-bits` (NaN payload preserved).
- References are runtime `func-instance` objects or `nil` (null).

## Consequences

+: One canonical domain; no signed/unsigned representation bugs at
   subsystem boundaries; comparisons and equality are ordinary `=`.
+: i64 fits Lisp integers natively (bignum-backed on 64-bit SBCL);
   modular arithmetic via `logand` is exact.
−: Inner loops box i64s above 2^63-1 on SBCL (bignum escape). docs/design/PLAN.md
   §13 performance work (machine-word unboxing) is future M6 work; the
   compiled backend accepts this cost for correctness first.
−: Host functions must normalize (`i32-wrap`) returned values; the
   API documents this contract.
