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
   *Superseded in part (loops 18–24 + M7 slices 1–5 + M8 + M9 + M10 + M11a, 2026-10-26): tail calls
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
   Component/WASI, threads, FASL cache. M11a (2026-10-26: the
   ASYNC CORE — the task/thread state machine per
   CanonicalABI.md Tasks 569-825: the callback event loop
   (the real-park YIELD — the always-true hot-spin replaced
   with a condition-wait; the exclusive lock released around
   both the YIELD park and the WAIT, per 3470-3483), the
   backpressure gate with pre-park scheduler registration,
   the deadlock oracle (the progress counter + the
   quiescent-cycle trap, 3539-3542), the async built-ins as
   real host funcs (task.return/cancel, context.get/set,
   backpressure.{inc,dec}, waitable-set.{new,wait,poll,
   drop}, waitable.join, subtask.{cancel,drop} with the exact
   trap messages incl. the sync-task block trap 3522-3543),
   the packed async lower with the STARTED-wait (3707) and
   cross-thread wake propagation at the resolution sites) is
   implemented; the component lane whitelists the 7
   async-core-green files and measures 1031 pass / 0 fail / 0
   unsupported / 0 harness-error of 1031 — FULLY GREEN. The
   remaining 38 async files stay in nyi.txt with per-file
   probe evidence (37 need the M11b canons — future/stream/
   thread/error-context; async-calls-sync has one open
   interleave bug). M11b (2026-10-27: THE CANON FAMILIES —
   the End record family per CanonicalABI.md 1491-1922 +
   4242-4534: {stream,future}.new/read/write with the five-
   case End.copy (1687-1711) + the bounded-buffer
   backpressure + the BLOCKED sentinel, cancel-{read,write}
   (4376), drop-{readable,writable} (4448), forward (4495:
   the outer-link + the both-pending corner 1760 + the
   empty-element take where a 0-byte element still advances
   the progress by the count); the Table free list (1142-
   1148: remove appends, add pops first); the THREAD core
   (4535-4769: index/yield/suspend/resume-later + new-
   indirect's suspended spawn-thunk threads with the 4587
   trap handler + the four -then-{resume,promote} variants
   4668-4770); the three-shape STARTED semantics (3651:
   sync-typed inline, callback at the initial return or the
   wset.wait park, stackful at on_start — derived from four
   fixtures' expectations, state-check-verified); the sync-
   lower's two-sided type/handle split (the result type
   resolves against the callee, the handle indices against
   the caller — the positional walk, zero-length's fix); the
   trap propagation (3584: a trapped callee re-signals at
   the boundary — the sync caller loop + the async delivery
   closure, cancel-not-delivered's fix); the {stream,future}.
   forward kind validation; the async dynamic context
   proclaimed at the file head — a binding site preceding
   the defparameters compiled LEXICAL and never reached the
   guest arms, the sessions-21-23 binding-visibility
   mystery) is implemented; the component lane whitelists
   25 async files and measures 1296 pass / 0 fail / 0
   unsupported / 0 harness-error — FULLY GREEN. M12
   (2026-10-27: THE SCHEDULER SEMANTICS — the nested-sync-call
   handshake per CanonicalABI.md 572 + 3538-3543: every
   sync-typed lift runs its core call through a FRESH
   executor SBCL thread bound to the per-call Task+implicit-
   Thread, the caller DRAINING at the top level (resume-all +
   poll + the deadlock oracle at 200 idle cycles) or PARKING
   when nested — the spec's non-async drain loop; the
   TWO-PARK split per 494 vs 523 — thread.suspend parks with
   NO readiness on the suspend waitqueue (only an EXPLICIT
   resume may wake it; Store.tick's resume-all does NOT)
   while thread.yield parks READY (any nudge re-runs it) —
   with the NUDGE/RESUME split (the spawn-thunk firing moved
   out of the nudge into resume only per 4590: a
   thread.new-indirect thread is created SUSPENDED and a bare
   tick can never start it — the no-sibling-resume stall's
   root cause, probe-confirmed); the RESUME-LATER TRAPS per
   4605-4610 (the unknown-index OOB trap + the
   not-suspended trap — the silent (when target ...) skip
   that dropped wakes is gone); the ZOMBIE-JOIN TEARDOWNS at
   both oracle paths per 3552 (the thread marked ABANDONED —
   its three park sites unwind via the async-thread-
   abandoned condition — then unregistered, unblocked,
   JOINed: a store cannot leak its parked threads); and the
   ATOMIC PACK EXIT (the STARTED-wait's exit and the state
   read captured TOGETHER under the callee task's mutex; the
   exit-to-read window closed by construction). Two pins
   cover the mechanisms (sync-lift-thread-context +
   suspend-vs-yield-determinism). The lane whitelists the
   SAME 25 async files plus the during-sync family (4
   fixtures, 52 rows) + trap-if-block-and-sync (48 rows) and
   measures 1396 pass / 0 fail / 0 unsupported / 0
   harness-error — FULLY GREEN, +100 rows over M11b. THE
   RECORDED DESIGN EVIDENCE (the negative-result chain, M12
   sessions 6-12, all measured and reverted per the
   one-change-per-gate rule): the post-OnStart yield-park
   (regressed reentrance cmd14 7/8), the before-OnStart
   pack-mark hold (deadlocked the nested-async pins — the
   wait's own STARTED exit vs the held OnStart tie), the
   post-OnStart bare-spin (regressed cmd14 8/8), the
   no-wait pack + OnStart hold (regressed cmd14+cmd18+
   cmd20 all 8/8 — and the correction: the STARTED-wait IS
   the spec's embedder freedom, its three exits mapping
   exactly to the three fixture shapes: resolved -> the
   eager RETURNED(2); state!=0 -> STARTED(1) — the
   sync-inline AND the async-callback callee whose OnStart
   fires before its WAIT-park; gate-parked -> STARTING(0)),
   and the atomic exit (closed the read window; changed
   nothing — kept as strictly-safer). CONCLUSION: the
   residual is the genuine EMBEDDER-CHOICE problem needing
   the full TOKEN SCHEDULER (one un-parked guest turn at a
   time, the tick as the only grant) — the next milestone's
   opening problem. The remaining nyi ladder with per-file
   probe evidence: the sequential-schedule family
   (async-calls-sync 3, reentrance 25 — the token-design
   hand-off), cancel-targeted-resume 10, cancel-subtask 4,
   cancel-delivery 4, cancel-resumed-callback-switch 4,
   sync-streams/partial-stream-copies/
   cancel-and-exclusive-lock (single rows), cross-abi-calls
   49, big-interleaving-test 57, idle-drop 38,
   passing-resources 3, switch-to-ready-callback 17,
   sync-barges-in 3, drop-cross-task-borrow 7, and T10's
   non-oracle completion-path leak (4 threads at reentrance
   cmd22). M13 (2026-10-28: THE TOKEN SCHEDULER — the aging quantum
   per CanonicalABI.md 922-941: ONE grant per tick in the
   creation-order path sort (the deterministic interleave the
   sequential-schedule family requires), the per-thread
   skip-age escape at a threshold UNDER the deadlock
   oracle's idle budget (40 < 50: the starved thread takes
   the grant before the oracle fires), the priority pass (the
   ready-marked suspend-parked/spawn-pending), the
   whole-store teardowns (the oracle's abandon-and-join AND
   the between-script boundary — a script's parked threads
   never leak into the next script's grants), the
   *print-circle* harness guard (the cyclic async-end
   printer explosion), and the inter-round drain (the
   suspend-then-resume arm drains the surviving wake count
   under the mutex before arming each round — the previous
   round's late post can no longer wake the next round
   early; the no-sibling resume window closed 0/50); THE
   RESOLVE-STASH WINDOW (the trap rides the subtask's
   flat-results ATOMICALLY with the state — the caller's
   wset delivery never sees a resolved subtask without its
   trap rider; the cancel-not-delivered ~29% flake dead);
   THE TEARDOWN JOIN (the abandoned threads' SBCL deaths
   complete before the next script starts — the
   unwind-storm interference class); and THE OWN-TRANSFER
   CHAIN (the stream-new canon's element single-resolution
   per 4242-4277; the take/put own-transfer at the copy
   boundary per 1687-1711 — the resource crosses the stream
   WITH its rep; the guest-body borrow conversion per 2412 —
   the callee's core receives h.rep; the resource builtin's
   borrow miss-walk per 3783 — the cross-task drop works
   through the borrowed record; passing-resources' 3 rows
   joined the lane). The lane whitelists 27 async fixtures
   and measures 1449 pass / 0 fail / 0 unsupported / 0
   harness-error — FULLY GREEN, +53 rows over M12's 1396
   (the acs 3 + the passing-resources 3 landed; the
   switch-to-ready 17 + the no-sibling 6 parked). THE M13
   STABILITY SPRINT'S RECORDED DESIGN EVIDENCE (the
   negative-result chain, all measured and reverted per the
   one-change-per-gate rule): the sync-delivery wait (the
   cancellable+lock-free condition made reentrance 38/38 but
   self-deadlocked the gated-callee round — the caller's
   own future-write dependency), the on-start deferral (the
   pack half worked — the STARTING report achieved — but
   the get-thread exit never completed under it), the
   live-task gate (the instance's live-count over-blocked
   the concurrent blocking-calls), and the ancestor gate
   (the caller-path prefix test held the fixtures but the
   acs release window admitted anyway; batch 6/10 vs the
   revert's floor class) — plus the COMPLETE WINDOW
   CAPTURES (the no-sibling incomplete pair; the
   switch-to-ready no-park; the acs missing (1 3) delivery
   with the release-race surface: the first pair's
   yield-park 3470 release vs the caller's continuation —
   the STARTED-wait guarantees the callee's START, not its
   park). CONCLUSION: the timing-window residual (acs's
   accepted ~8-10% rate recorded; switch-to-ready +
   no-sibling parked with their rates) is the M14 HAND-OFF:
   the token-scheduler determinism program. Still out of
   scope: the timing-window family's full closure, WIT-
   level Component/WASI, threads beyond the canon core,
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

   M14 (2026-10-28: THE SYNCHRONOUS RESUME + THE PACK WINDOW +
   THE GRANTED-PARK MODEL). §761-770/§474-484's synchronous resume:
   subtask.cancel's OnCancel resumes a cancellable-and-ready
   callback callee and observes its TURN BOUNDARY (resolved, turn
   moved, or the mid-turn re-park — the "blocks" half). The PACK
   WINDOW closes the keeper race: the flip-time pack (§3715), the
   callee-only spin, resume_later's mark-only arm (§462-465), the
   spawn-fire suppression, and the cancel-wait window's M11a
   deferral at its five sites. THE GRANTED-PARK MODEL (§932): a
   continuation is a GRANT from a real tick or an explicit resume,
   never an incidental broadcast; with the per-instance
   cancel-window gate for the drain's mid-window ticks. THE
   NEGATIVE-RESULT CHAIN: the §3483 candidates (the arm-time drain:
   3/4 unreachable; the real-wake loop: DEADLOCK 4/4 — the
   resume-all is the only waker at the row's end) and the
   whole-store suppression (acs::2 + ctr::5 regressions).
   M15 (2026-10-28: THE ASYNC SUITE COMPLETE — 45/45 files, lane
   1631/0/0/0, units 414, core 21,187/0/73). THE MECHANISMS, with
   their spec sections: (1) the granted park = GRANT **AND**
   READINESS (§932 one thread one quantum + §1373's wait-from-
   callback readiness — a grant spent while not ready re-arms); (2)
   the narrowed grant wake (only an ARMED park is woken — the broad
   wake regressed the during-sync family); (3) THE SYNC RENDEZVOUS
   (§932's "tick when the caller blocks" + the fixture's "switch to
   the caller": a blocking sync read/write continues at the caller's
   block, with %end-notify's DURABLE sync-waiter wake); (4) the
   TWO-SPACE RESULT STORE (the async lower's result valtype in the
   callee's type space, the new stream handle in the caller's
   table); (5) THE CANCEL-WINDOW FIRE GATE (the priority pass + the
   no-quantum fallback + the fifth M11a deferral at the async
   lower's on-resolve); (6) THE GATE'S PENDING-CANCEL WAKE (§681 +
   §761's deliver_pending_cancel: CANCELLED_BEFORE_STARTED without
   the backpressure subsiding — big-interleaving's command-52 hang);
   (7) the SYNC-IO park's ARM-TIME CLEAR (a pre-arm grant is stale
   for the IO park; the callee's own on-resolve grants at
   task.return time); (8) THE BLOCK DEPTH COUNTER (a counter, not a
   boolean; sb-ext:atomic-incf requires an (unsigned-byte 64) slot)
   with paired enter/exit at every park site and zeroing at every
   abandon sweep; (9) THE GUARDED MARKER — the raw type-error
   class's exact site: %sync-thunk-run's nested executor binds
   neither *current-task* nor *current-thread*, so an unguarded
   slot accessor there signals "NIL is not of type ASYNC-THREAD"
   and escapes as a component trap; (10) THE REFINED DRAIN
   PREDICATE ("a tick can help => spend it": the root parked, a
   yield-park, a ready-mark, or an armed non-IO park whose
   readiness holds; IO parks belong to the root's block tick); (11)
   THE ORACLE'S QUIESCENCE RULE (a spent tick with no progress is
   a quiescent cycle — deadlock.wast's oracle). THE NEGATIVE-RESULT
   CHAIN, four gate shapes measured and three reverted: the broad-
   wake gate (during-sync regressions + a lane hang); the first-
   turn pack boundary (§3650-3729 — DEADLOCKs async-calls-sync: the
   callee's first turn is a nested sync call whose block was
   unobservable); the per-thread gate (lane hang at
   during-sync-scheduling-candidates); and the refined predicate
   (landed). THE SCOPE ADVANCE: every async file the suite vendors
   is in the lane; the residual is the PROBE METRIC's timing-pinned
   rows (async-calls-sync ::1 ~7-10% ACCEPTED, switch-to-ready ::8
   ~11% pre-M15, during-sync-scheduling-candidates — one failing row in 8/8
   repeated standalone runs at the pre-gate-10 base vs 6/8 at gate
   10 (pre-existing, improved by the gate; masked in-lane), cancel-targeted-resume ::9,
   sync-streams ::1, reentrance ::14, big-interleaving ::24) —
   the robustness target. Still out of scope: those rows' full
   closure, WIT-level Component/WASI, threads beyond the canon
   core, FASL cache. `declared_support` stays `partial`.

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
