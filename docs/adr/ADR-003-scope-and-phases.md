# ADR-003: Executable scope, phase separation, guest-recursion guard

Date: 2026-10-23 · Status: accepted

## Context

PLAN.md describes a 78–123 engineer-week program (M0–M12) with hard
conformance gates. This repository delivers an honest **partial** slice
(PLAN.md §2: `partial` = "some missing"; never advertise more).

## Decision

1. **Scope**: core Wasm MVP + bulk-memory + sign-extension +
   non-trapping-float-to-int + multi-value + reference types, end to
   end (decode → validate → interpret → compile → invoke). SIMD/GC/EH/
   tail-calls/memory64/WAT/Component/WASI/threads/FASL cache are out of
   scope and rejected with clear errors, not silently.
   *Superseded in part (loops 18–21, 2026-10-24): tail calls
   (`return_call`/`return_call_indirect`, constant-stack trampoline),
   exception handling (tags, `try_table`, `throw`, named module
   definition/instance forms), typed function references
   (`(ref $t)`/`(ref null $t)` lattice, `call_ref`,
   `return_call_ref`, `br_on_null`/`br_on_non_null`,
   `ref.as_non_null`, definite initialization), `rec` type
   groups with canonicalization + iso-recursive equivalence
   (the wasm-comptype/rectype-group engine), and struct/array
   TYPE declarations are now implemented end to end — the vendored
   core suite is fully attributed (0 fails; the 74 unsupported
   are the unadopted-annotations proposal + one SIMD fixture).
   Still out of scope: GC value OPCODES, SIMD, memory64,
   multi-memory runtime, WAT-level Component/WASI, threads,
   FASL cache. `declared_support` stays `partial`.*
2. **Failure phases are distinct conditions** (PLAN.md §12.2):
   `decode-error` → `validation-error` → `link-error` →
   `instantiation-error` → `wasm-trap`. A decode error can never be
   reported as validation, a link error never as a trap.
3. **Guest recursion must not kill the host**: the interpreter counts
   call depth and traps `:call-stack-exhausted` (limit ~1500) instead of
   exhausting the SBCL control stack (a fatal, uncatchable error).
4. **No third-party deps**; ASDF for loading only.

## Consequences

+: The public support declaration stays honest; `partial` per PLAN.md §2.
+: Test suite asserts both positive behavior and exact condition types
   and trap kinds.
−: `:call-stack-exhausted` limit is an implementation limit (PLAN.md
   §3.4) — documented, deterministic; deep-but-finite guest recursion
   beyond the limit is rejected as a resource limit, never a crash.
−: Modules using out-of-scope features fail at decode/validation with
   "unknown type form"/"unknown opcode" rather than a feature-gated
   message; acceptable for a partial slice, to be refined per-feature
   when the features land.
