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
   *Superseded in part (loops 18–24 + M7 slices 1–5 + M8 + M9 + M10, 2026-10-25): tail calls
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
   38 unsupported / 16 harness-error of 639. M7 slice 3
   (2026-10-25: the crash/shape families — the with-arg projection
   disambiguation, moduletype externtype interning, inverted-canon
   targets, elem-size alignment fixes; the semver/canonical-version
   grammar per Explainer 2787-3001 + upstream wasmparser; the
   declarator inner-first outer-fallback resolution; structural
   instantiation type matching with per-component type identities
   (fresh/ref/ct/opaque + with-arg substitution); and the
   validator-extension families — externname visibility,
   annotated-name shapes, canonopt unicity, moduletype internals)
   is implemented; the component lane measures 492 pass / 110
   fail / 37 unsupported / 0 harness-error of 639. M7 slice 4
   (2026-10-25: the parser type-use/bind gaps — type-use
   externtypes for every sort, declarator bind-ids with
   sort-correct spaces + type indices, the type-decl-sink/
   intern-scope machinery, moduletype desc shapes with the
   shared flag; and the validator's core-subtype moduletype
   matching over the static core-IR resolver, the canon
   core-functype signature checks via the flat-types builder,
   and the visibility table propagation) is implemented; the
   component lane measures 531 pass / 71 fail / 37
   unsupported / 0 harness-error of 639. M7 slice 5 (2026-10-25:
   the canonopt string-encoding conflicts; the outer-alias de
   Bruijn scope stack + the resource-transitivity walk + the
   declarator sort restriction; the sort-aware binding table;
   moduletype named aliases; the resource identity/generativity/
   local-resource/dtor/borrow-transitivity canon rules; the
   type-use visibility walks + projection chains + the
   export-ascription compatibility; the annotated resource
   names; the inline-functype shape for inverted lifts; the
   imported component spaces + canon core-func alloc ordering +
   moduletype inner rules + core instantiation rules + builtin
   immediates + inline target sugar; the *outer-states* special
   binding + nested-type outer threading) is implemented; the
   component lane measures 603 pass / 0 fail / 36 unsupported /
   0 harness-error of 639 — ZERO FAILS; the 36 unsupported are
   valid binaries awaiting instantiation execution. M8 (2026-10-25:
   component instantiation execution — src/component-runtime.lisp,
   the comp-instance runtime: two-level core-instance linking
   per Explainer 277-300, component-instance with-args, alias
   projection, canon lift with the flat canonical call path,
   start funcs last; wired into the runner through the
   *component-instantiator* hook with link-error -> :link and
   wasm-trap -> :execute classification) is implemented; the
   component lane measures 639 pass / 0 fail / 0 unsupported /
   0 harness-error of 639 — FULLY GREEN, every row passes.
   Still out of scope: the non-flat canonical ABI at runtime
   (strings/lists/records via memory + realloc — the values
   suite), resource handle runtimes, the linking suite's
   remaining files, async, WIT-level Component/WASI, threads,
   FASL cache. M9 (2026-10-25: the LINKING suite — canon
   lower as real host core funcs; resource built-ins with
   per-instance handle tables + own/borrow transfer
   (lift_own/lower_own/borrow-rep + dtor resolution); the
   imported-instance identity machinery (declarator-space
   remaps + per-slot skip vectors + the with-args
   substitution); the outer-aliased core module resolution +
   the export-alias chains + the ascribed-type projections;
   EH tag identity resolved at try_table entry; component
   invoke/get in the runner) is implemented; the component
   lane now runs {binary,validation,linking} and measures
   911 pass / 0 fail / 0 unsupported / 0 harness-error of
   911 — FULLY GREEN, every row passes. M10 (2026-10-25: the
   VALUES suite — the canonical ABI at runtime:
   cx.allocate/reallocate with the realloc trap taxonomy,
   store/load per type (string transcodes utf8/utf16/
   latin1+utf16; list/record/variant/flags/enum/option/
   result; the map despecialization to list-of-tuples per
   CanonicalABI.md 1938), lift/lower_flat + the MAX_FLAT
   16/1 pointer indirection, post-return, the compound
   value literals + the named (component instance $i $X)
   script command; the canon lower's OWN canonopts drive
   the wrapper's ABI memory; the declarator-space entry-at
   remap counts every declarator) is implemented; the
   component lane now runs {binary,validation,linking,
   values} and measures 1015 pass / 0 fail / 0 unsupported /
   0 harness-error of 1015 — FULLY GREEN, every row passes
   (alignment 25, concat 46, numerics 26, realloc 16,
   strings 17, transcode 10). nyi.txt: values/post-return
   + values/variants (the async task.return/may_leave
   machinery) + validation/kebab (the vendored exclusion,
   now effective via the nyi-p matcher fix). Still out of
   scope: async (the task state machine), WIT-level
   Component/WASI, threads, FASL cache.
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
