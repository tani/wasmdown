# ADR-003: Executable scope, phase separation, guest-recursion guard

Date: 2026-10-23 · Status: accepted

## Context

docs/design/PLAN.md describes a 78–123 engineer-week program (M0–M12) with hard
conformance gates. This repository delivers an honest **partial** slice
(docs/design/PLAN.md §2: `partial` = "some missing"; never advertise more).

## Decision

1. **Scope**: core Wasm MVP + bulk-memory + sign-extension +
   non-trapping-float-to-int + multi-value + reference types, end to
   end (decode → validate → interpret → compile → invoke). SIMD/GC/EH/
   tail-calls/memory64/WAT/Component/WASI/threads/FASL cache are out of
   scope and rejected with clear errors, not silently.
   *Naming convention: the engine-slice feature loops are tracked by loop
   number; the Component Model work is tracked by docs/design/PLAN.md §14
   milestone id — the two counters never renumber each other.*

   *Superseded in part.* Almost everything item 1 excluded has since
   landed: tail calls, exception handling, typed function references,
   `rec` groups, GC values, multi-memory and memory64, SIMD, the
   `.wat`/`.wast` front ends, the Component Model (M7–M17) and the
   WASI 0.3.2 providers. The cycle-by-cycle record — mechanisms, spec
   sections, pins and measured rates — is archived in
   [`../history.md`](../history.md); current coverage is
   [`../status.md`](../status.md). The parts of this decision still in
   force are the phase separation (item 2) and the recursion guard
   (item 3).
2. **Failure phases are distinct conditions** (docs/design/PLAN.md §12.2):
   `decode-error` → `validation-error` → `link-error` →
   `instantiation-error` → `wasm-trap`. A decode error can never be
   reported as validation, a link error never as a trap.
3. **Guest recursion must not kill the host**: the interpreter counts
   call depth and traps `:call-stack-exhausted` instead of exhausting the
   SBCL control stack (a fatal, uncatchable error). The current limit is
   `wasmdown.interp:*call-depth-limit*`; see
   [`../usage/errors.md`](../usage/errors.md).
4. **No third-party deps**; ASDF for loading only.

## Consequences

+: The public support declaration stays honest; `partial` per docs/design/PLAN.md §2.
+: Test suite asserts both positive behavior and exact condition types
   and trap kinds.
+: WASI hosts receive explicit authority objects only (no ambient
   globals); the filesystem escape suite and the network policy matrix
   pin those invariants as executable tests.
−: `:call-stack-exhausted` limit is an implementation limit (docs/design/PLAN.md
   §3.4) — documented, deterministic; deep-but-finite guest recursion
   beyond the limit is rejected as a resource limit, never a crash.
−: Modules using out-of-scope features fail at decode/validation with
   "unknown type form"/"unknown opcode" rather than a feature-gated
   message; acceptable for a partial slice, to be refined per-feature
   when the features land.

*Lapsed consequences (not rewritten, recorded here):* at acceptance this
ADR also carried an HTTP transport and a no-custom-TLS rule
(`https` without an embedder transport = the WIT's `configuration-error`).
WASI HTTP was later removed and is out of scope — there is no provider and
no transport option — see [`../usage/wasi.md`](../usage/wasi.md).
