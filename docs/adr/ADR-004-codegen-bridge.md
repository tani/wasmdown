# ADR-004: Interpreter/codegen bridge via the wasm-call trampoline

Date: 2026-10-23 · Status: accepted

## Context

PLAN.md §6.1/§12.3: interpreter and compiled paths must coexist, be
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

## Consequences

+: Interp and codegen can be parity-tested on identical instances
   (PLAN.md §12.3 invariant).
+: No global-mode switching; an embedder can hold both.
−: Compiled→guest-wasm calls pay one extra `funcall` vs a hypothetical
   direct-compiled edge; acceptable at baseline tier, revisit at M6.
