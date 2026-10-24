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
   *Naming convention: the engine-slice feature loops (18–24) are
   tracked by loop number; the Component Model work is tracked by
   PLAN.md §14 milestone id (M7 slices) — the two counters never
   renumber each other.*
   *Superseded in part (loops 18–24 + M7 slices 1–2, 2026-10-25): tail calls
   (`return_call`/`return_call_indirect`, constant-stack trampoline),
   exception handling (tags, `try_table`, `throw`, named module
   definition/instance forms), typed function references
   (`(ref $t)`/`(ref null $t)` lattice, `call_ref`,
   `return_call_ref`, `br_on_null`/`br_on_non_null`,
   `ref.as_non_null`, definite initialization), `rec` type
   groups with canonicalization + iso-recursive equivalence
   (the wasm-comptype/rectype-group engine), GC values END
   TO END (struct/array declarations AND the full opcode surface
   — struct.new/get/set with packed normalization, the array
   family, i31, ref.cast/ref.test/br_on_cast, ref.eq, the
   any↔extern conversions — self-authored tests per PLAN.md
   §12, since no vendored fixture exists), and multi-memory
   runtime + memory64 (memidx immediates on every memory op,
   cross-memory copy with bounds on both backings, i64
   addressing with the addrtype-typed size/grow/fill/init
   implemented, and M7 slice 1 (Component Model foundation: the
   component binary reader — layer discrimination, all sections,
   WIT value types incl. map 0x63, aliases, canon, values — plus
   the label/index/max-size validation rules and the
   `make test-component` lane against the vendored suite) is
   implemented. The vendored core suite is fully attributed
   (0 fails; the 73 unsupported are the unadopted-annotations
   proposal). M7 slice 2 (2026-10-25: the component TEXT parser
   — src/component-text.lisp, the full text grammar into the
   binary reader's IR; the validator's canonopt lift/lower rules
   per CanonicalABI.md, the words/remove-annotations externname
   grammar, imported/exported type placeholders, outer-alias
   count+target validation, nested-component recursion) is
   implemented; the component lane measures 305 pass / 280 fail /
   38 unsupported / 16 harness-error of 639. Still out of scope:
   the validator-extension families (instantiation arg/type
   matching, version-suffix semver, external-visibility,
   annotated-name shapes), instantiation/linking, the canonical
   ABI, resources, async, WIT-level Component/WASI, threads,
   FASL cache.
   `declared_support` stays `partial`.*
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
