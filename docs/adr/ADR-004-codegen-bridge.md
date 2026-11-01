# ADR-004: Interpreter/codegen bridge via the wasm-call trampoline

Date: 2026-10-23 · Status: accepted

## Context

docs/design/PLAN.md §6.1/§12.3: interpreter and compiled paths must coexist, be
differential-equivalent observables, and share semantics — including
host functions — without one clobbering the other.

## Decision

- `wasmdown.runtime:wasm-call` is the single call trampoline:
  `:host` func-instances → `(funcall host-fn args)`; `:wasm`
  func-instances → `(funcall *invoke-wasm-func* func args)`.
- The interpreter installs itself into `*invoke-wasm-func*` at load.
- Compiled functions integrate as **closures shaped like host
  functions** `(lambda (args) results-list)` — they do not replace the
  global interpreter hook. Mixed call graphs (guest calls compiled fn
  calls interpreted fn) flow through one dispatch point.
- Const-expr evaluation at instantiation goes through the analogous
  `*const-expr-evaluator*` hook.

## Amendment (2026-11-01)

*Compiled functions no longer enter as replacement host func-instances.*
`func-instance` now carries `backend` and `compiled-entry`; instantiation
fills the entry on the defined function's **existing** `:wasm` reference,
so exports, tables, element segments, `ref.func` constants and other
modules' imports keep the same object, and an imported function keeps the
backend of its defining instance. The single call point is still
`wasmdown.runtime:wasm-call`, now through `invoke-wasm`, which also owns
guest depth, the tail-transfer trampoline and the outermost exception
boundary for both backends. The consequence noted below (one extra
`funcall` on a compiled→guest edge) is unchanged. The migration record is
in [`../history.md`](../history.md); current behaviour is documented in
[`../usage/lisp-api.md`](../usage/lisp-api.md). The decision above stands;
this note records the mechanism it now takes.

## Consequences

+: Interp and codegen can be parity-tested on identical instances
   (docs/design/PLAN.md §12.3 invariant).
+: No global-mode switching; an embedder can hold both.
−: Compiled→guest-wasm calls pay one extra `funcall` vs a hypothetical
   direct-compiled edge; acceptable at the compiled tier, revisit with an optimizing tier.
