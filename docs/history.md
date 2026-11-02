# History

> An archive of engineering notes kept for provenance — what landed when, which
> mechanism a measured behaviour depended on, and what was known to be shaky.
> It is **not** the current status: see [`status.md`](status.md) for that and
> [`conformance/`](conformance/README.md) for the lane numbers as they stand.

Distilled from the repository README before the documentation restructure, plus
`specs/lock.toml` and the generated reports. Ids are [`design/PLAN.md` §14](design/PLAN.md)'s
Component Model milestones; engine-slice feature work was tracked by *loop*
number. The two series never renumber each other.

## Milestones at a glance

| Id | What landed |
| --- | --- |
| M7 | Component front end: binary reader (all 12 sections), validator rule set, component text parser, the `(component …)` script family, `make test-component`. |
| M8 | Component instantiation *execution*: the `comp-instance` runtime, two-level core linking, `with-args`, aliases, canon lift, start functions. |
| M9 | Linking suite: canon lower as real host functions, resource built-ins with per-instance handle tables, imported-instance identity, outer-aliased core resolution, export-alias chains, ascribed-type projections, EH tag identity, `invoke`/`get` in the runner. |
| M10 | Values suite: the canonical ABI at runtime — string/list/record/variant/map lowering through `realloc` and guest memory, flattening, the `MAX_FLAT` pointer indirection. |
| M11a | Async core: task/thread state machine, callback event loop, backpressure gate, deadlock oracle, async built-ins as real host functions, packed async lower with its `STARTED` wait. |
| M11b | Canon families: the End record family for stream/future, the table free list, the thread core, the three `STARTED` shapes, sync-lower's type/handle split. |
| M12 | Scheduler semantics: nested-sync-call handshake, the suspend-vs-yield two-park split, the nudge/resume split, resume-later traps, zombie-join teardown, the atomic pack exit. |
| M13 | Token scheduler and own-transfer chain: the aging quantum, one grant per tick in creation-order path sort, the resolve-stash window, teardown joins, the inter-round drain. |
| M14 / M15 | Scheduler determinism program (mechanisms below). Two fixes landed, four candidates were measured and reverted; close: lane metric 1,631/0/0/0 reproduced ×3, probe metric still flaky. |
| M16 | WASI 0.3.2 host providers: vendored WIT snapshot and reader, host-provider framework, host stream/future ABI, five package providers, the capability model, the CLI command runner. |
| M17 | Close-out: component lane at 0 fail × 30 runs, WASI HTTP provider removed at user direction, descriptor lifetime reference counting, deletion of the canon-lower stream/future arms. |
| Loop 18 | Tail calls (`return_call`, `return_call_indirect`), exception handling, named module definitions and instances. |
| Loop 19 | Typed function references (`call_ref`, `return_call_ref`, `br_on_null`, `br_on_non_null`, `ref.as_non_null`). |
| Loop 20 | `rec` type groups with canonicalization and iso-recursive equivalence. |
| Loop 21 | `struct`/`array` type declarations. |
| Loop 22 | GC value opcodes end to end: struct/array new/get/set, `i31`, casts, heap lattice, `any`↔`extern`. |
| Loop 23 | Multi-memory runtime and memory64 end to end. |
| Loop 24 | SIMD: the 236-op v128 family and 20 relaxed ops, scalarized, with 32 self-authored pins. |

## M7 — the component front end

- **Layer discrimination**: magic, version `0x0D` and the layer byte decide core module vs component; `wasmdown.binary` owns the decision, `src/component.lisp` consumes it.
- **Binary reader** (`src/component.lisp`): all 12 sections, WIT value types including `map` (`0x63`), aliases, canon entries including all 47 async built-ins, and the value sections. Decoding discipline mirrors `src/binary.lisp` — every read bounds-checked, failure phase `decode-error` with a byte offset.
- **Validator**: label/index/max-size rules; `canonopt` lift/lower with unicity and signature checks; semver and canonical-version grammar; externname visibility plus type-use walks; annotated resource names; moduletype internals; structural instantiation type matching with per-component type identities and generativity; core-subtype moduletype matching; the de Bruijn outer-alias stack; resource identity/local/canon rules; projection chains.
- **Text parser** (`src/component-text.lisp`): the full component grammar into the same IR — valtypes, functypes, imports/exports with attributes, canon lift/lower and built-ins upright and inverted, core modules via `parse-wat`, core instances, aliases (outer-alias sugar, projections, declarator bind-ids, type-use externtypes, the sort-aware binding table, moduletype desc shapes), and the named form `(component definition …)`.
- **M7-era measurement**, kept because it shows where the lane started: 305 pass / 280 fail / 38 unsupported / 16 harness-error of 639 commands over the 75 vendored files. The 280 fails were the instantiation/arg-matching, version-suffix, external-visibility and annotated-name-shape validator-extension families — the next slice's work, not a regression.
- **How the lane reached zero fails** (slices 2–5, 2026-10-25), the part later slices paid for most: the component text grammar and the `canonopt` lift/lower rules per `CanonicalABI.md`, including the `words`/`remove-annotations` externname grammar, imported/exported type placeholders, outer-alias count+target validation and nested-component recursion (**492** pass / 110 fail / 37 unsupported / 0 harness-error); the crash/shape families — `with-arg` projection disambiguation, moduletype externtype interning, inverted-canon targets, elem-size alignment, the semver/canonical-version grammar per Explainer 2787–3001, and declarator inner-first/outer-fallback resolution (**531** / 71 / 37 / 0); then type-use externtypes for every sort, declarator bind-ids with sort-correct spaces and type indices, moduletype desc shapes, core-subtype moduletype matching over the static core-IR resolver, and canon core-functype signature checks via the flat-types builder; and finally the canonopt string-encoding conflicts, the outer-alias de Bruijn scope stack with the resource-transitivity walk, resource identity/generativity/`dtor`/borrow-transitivity, the sort-aware binding table, imported component spaces, core-func alloc ordering and the `*outer-states*` special binding — **603** / **0** fail / 36 unsupported / 0 harness-error of 639. The 36 unsupported were valid binaries awaiting instantiation execution, which M8 then supplied.

## M8–M10 — instantiation, linking, canonical ABI

- **M8, the `comp-instance` runtime** (`src/component-runtime.lisp`): per-sort index vectors in validator order plus import/export tables; two-level core-instance linking; component-instance `with-args`; alias projection; canon lift/lower as real host functions; resource built-ins with per-instance handle tables, own/borrow transfer and destructor resolution; outer-aliased core module resolution; the per-instantiation closure side table; start functions.
- **M9, linking**: the imported-instance identity machinery, export-alias chains, ascribed-type projections, exception-handling tag identity across instances, and `invoke`/`get` in the component runner.
- **M10, canonical ABI at runtime**: `cx.allocate`/`reallocate` with the trap taxonomy; store/load per type; string transcodes (UTF-8, UTF-16, Latin-1 + UTF-16); list/record/variant/flags/enum/option/result; map despecialization; `lift`/`lower_flat`; the `MAX_FLAT` values (16 and 1) and the pointer indirection above them; the lowered host function's *own* `canonopt` memory.

## M11a — the async core

- The task/thread state machine, with a callback event loop whose park is a real park: `YIELD` and an exclusive-lock-released `WAIT`.
- The backpressure gate and the deadlock oracle; the async built-ins as real host functions with the exact trap messages the suite expects; the packed async lower with its `STARTED` wait.

## M11b — the canon families

- **End record family**: `{stream,future}.new/read/write`, the five-case `End.copy` result, bounded-buffer backpressure with the `BLOCKED` sentinel, `cancel-{read,write}`, `drop-{readable,writable}`, and `forward` with its outer-link, both-pending corner and empty-element take.
- **Table free list**; **thread core** — `thread.index`, `thread.yield`, `thread.suspend`, `thread.resume-later`, plus `thread.new-indirect`'s suspended spawn-thunk threads with the §4587 trap handler and the four `-then-{resume,promote}` variants.
- Three-shape `STARTED` semantics; the sync-lower's two-sided type/handle split; trap propagation; `{stream,future}.forward` kind validation; the async dynamic context proclaimed at the file head.

## M12 — scheduler semantics

- **Nested-sync-call handshake**: every sync lift runs its core call through a *fresh* executor SBCL thread bound to the per-call Task and implicit Thread of §572; the caller *drains* at the top level (§3538–3543: resume-all, poll, deadlock oracle) or *parks* when nested.
- **Two-park split**: `thread.suspend` parks with no readiness on the suspend waitqueue (§494 — only an explicit resume may wake it; `Store.tick`'s resume-all does not), while `thread.yield` parks *ready* (§523 — any nudge re-runs it).
- **Nudge/resume split**: spawn-thunk firing moved out of the nudge into resume only, so a `thread.new-indirect` thread is created suspended per §4590 and a bare tick can never start it.
- **Resume-later traps** (§4605–4610): the unknown-index out-of-bounds trap and the not-suspended trap; the silent `(when target …)` skip that dropped wakes is gone.
- **Zombie-join teardowns**: both oracle paths — the sync-lift executor and the stackful-async guest — mark the thread `ABANDONED`; its three park sites unwind via the `async-thread-abandoned` condition, then unregister, unblock and join, per §3552's "trap and tear down the whole store".
- **Atomic pack exit**: the `STARTED` wait's exit and the state read are captured together under the callee task's mutex.
- Pins: `component/sync-lift-thread-context-pins` (a sync body's own thread index; a spawned resumer's `resume-later` completing a suspended invoke) and `component/suspend-vs-yield-determinism-pins`. M12 added the during-sync family (4 fixtures, 52 rows) and `trap-if-block-and-sync` (48 rows) — 100 rows over M11b's 1,296 — for 25 files in the lane.

## M13 — token scheduler and own-transfer chain

- **Aging quantum and token assembly** (§922–941): one grant per tick in the creation-order path sort, the deterministic interleave the sequential-schedule family requires; a per-thread skip-age escapes genuine starvation at a threshold below the deadlock oracle's idle budget; the priority pass; whole-store teardowns at both the oracle and the script boundary; a `*print-circle*` guard in the harness against the cyclic struct printer explosion.
- **Resolve-stash window closed**: the trap rides the subtask's flat-results atomically with the state, so the caller's wait-set delivery can never see a resolved subtask without its trap rider. **Teardown join**: abandoned threads' deaths complete before the next script starts, removing the unwind-storm interference.
- **Inter-round drain**: the suspend-then-resume arm drains the surviving wake count under the mutex before arming each round, so a previous round's late post cannot wake the next round early.
- **Own-transfer chain**: the `stream.new` canon's element single-resolution; the take/put own-transfer at the copy boundary, where the resource crosses the stream with its rep; the guest-body borrow conversion, where the callee's core receives the rep per §2412; and the resource built-in's borrow miss-walk that lets a cross-task drop work through the borrowed record. The three rows of `passing-resources` joined the lane on this.

## M14 / M15 — scheduler determinism

The mechanism model the async suite needed, all of it landed by the M15 close.
Each item was a rule the suite could detect the violation of; the parenthetical
is the spec section or the failure it was written against.

- **Synchronous resume** (§761–770, §474–484): `subtask.cancel`'s `on-cancel` resumes a cancellable-and-ready callback callee and observes its *turn boundary* — resolved, turn moved, or mid-turn re-park.
- **Pack window**, the keeper race: the flip-time pack, the callee-only spin, spawn-fire suppression, and the M11a deferral's four sites.
- **Granted-park model** (§932): a thread's continuation is a *grant*, set by a real tick or an explicit resume, never by an incidental broadcast, with the per-instance cancel-window gate. **Granted park = grant *and* readiness**: one grant is one quantum, and a grant spent while not ready re-arms. **Narrowed grant wake**: only an *armed* park is woken.
- **Sync rendezvous**: a blocking sync read/write continues at the caller's block — §932's "tick when the caller blocks" — carried by `%end-notify`'s durable sync-waiter wake.
- **Two-space result store**: the async lower's result valtype lives callee-side, the new handle caller-side.
- **Cancel-window fire gate**: the priority pass, the no-quantum fallback, and the fifth M11a deferral. **Gate's pending-cancel wake** (§681, §761): `CANCELLED_BEFORE_STARTED` without the backpressure having subsided.
- **Sync-IO park's arm-time clear**: a pre-arm grant is stale for the IO park.
- **Block depth counter**: a counter, not a boolean, and the atomic slots must be `(unsigned-byte 64)`; **guarded marker** — the raw type-error class's exact site, `%sync-thunk-run`'s nested executor binds no `*current-thread*`.
- **Refined drain predicate** ("a tick can help ⇒ spend it"): the root is parked, or a yield-park, a ready-mark, or an armed non-IO park is ready; IO parks belong to the root's block tick. **Oracle's quiescence rule**: a spent tick with no progress is quiescent.

The determinism program beneath M14: two fixes landed; four candidates were
measured and reverted — the sync-delivery candidate, the on-start deferral, the
live-task gate and the ancestor gate — and the window captures are the recorded
design evidence for those reverts.

### The honest rate map

Two metrics were tracked and they disagreed.

**Lane metric** — one fresh process per file: 1,631 pass / 0 fail / 0
unsupported / 0 harness-error, reproduced ×3 at the close.

**Probe metric** — each row standing alone, load-sensitive: the 10-run fail-row
census at the M15 close was 5/10 clean runs, with single non-passes on five
different rows. The recorded rows and rough rates:

| Row | Rate |
| --- | --- |
| `async-calls-sync` ::1 | ~7–10 % — the *accepted* residual |
| `switch-to-ready` ::8 | ~11 % before M15 |
| `cancel-targeted-resume` ::9 | intermittent |
| `sync-streams` ::1 | intermittent |
| `reentrance` ::14 | intermittent — the drift row |
| `big-interleaving` ::24 | intermittent |

`during-sync-scheduling-candidates` was flakier: one failing row in 8 of 8
repeated standalone runs at the pre-gate-10 base, and in 6 of 8 at gate 10 —
pre-existing, improved by gate 10, masked in-lane, and named the next
robustness target. The lane is *bimodal* on the reference box: earlier windows
on the same master measured 18, 9 and 1 fail commands. Waiting for a lucky
window was never the answer; the per-row pins, each failing when its rule is
re-broken, are the guarantee.

The residual `nyi` ladder carried per-file probe evidence and the window
captures: `switch-to-ready-callback` 17 (the ::8 no-park gate/`STARTED`-wait
shape, ~6–10 %), `during-sync-call-no-sibling-resume` 6 (the resume window,
closed by the inter-round drain, leaving an unknown-thread-index variant at
~3 %), `reentrance` 25 (the state-read race; the sync-delivery candidate was
measured and reverted on the cancel family's self-deadlock), `sync-streams` (the
`STARTING`/`STARTED` embedder race), `cancel-targeted-resume` 10,
`cancel-subtask` 4, `cancel-delivery` 4, `cancel-resumed-callback-switch` 4,
`partial-stream-copies`, `cross-abi-calls` 49, `big-interleaving-test` 57, and
`trap-if-block` ::34. `nyi.txt` also carries `values/post-return.wast` and
`values/variants.wast` (same async machinery) and `validation/kebab.wast` (a
vendored exclusion).

## M16 — WASI 0.3.2 host providers

- **Vendored snapshot**: `specs/wasi/wasi-wit-0.3.2/`, the official v0.3.2 release asset of 2026-10-13, verbatim — 15 files / 2,448 lines covering `cli`, `clocks`, `filesystem`, `random`, `sockets`, plus the release's own `*/deps/*.wit` copies. `specs/wasi/MANIFEST.md` records the URL, the tarball SHA-256 (`2e918e04…c0ec311`) and the re-verification recipe. Policy: exact stable snapshots only; version dispatch by the `@0.3.2` suffix in the import name; `@unstable(feature = …)` interfaces gated off by default.
- **WIT reader** (`src/wit.lisp`): tokenizer plus recursive-descent reader with type resolution and WIT → component-`ct` conversion. All six vendored packages parsed without error (`wit/all-six-vendored-packages-parse`); resource types carry a per-interface memo so `own<T>`/`borrow<T>` payloads share one stable handle identity.
- **Host-provider framework** (`src/wasi.lisp`): a parsed interface plus an implementation alist becomes a real `comp-instance` of `lifted-func`s with a `host` slot; `lifted-func-call` dispatches the CL host closure over the component *value domain*, so the provider speaks the domain and the surrounding `canon lower` lifts/lowers in the *caller's* memory. Providers arrive as instantiation `:with-args` under exact WIT names.
- **Host stream/future ABI**: `async-end` gained `host-sink`, `host-source`, `host-value`/`host-value-p`, `host-eof-p`/`host-error` and the one-shot `host-on-drop`; a `host-buffer` struct stands in for a guest linear-memory region so `%buffer-remain/-progress/-take/-put` keep one shape. Insertion points: `%end-copy`'s case 2 consults the peer before parking (`%host-sink-drain`, `%host-source-fill`, `%host-value-deliver`), `%end-drop` fires the detached peer's `host-on-drop`, `host-end-drain` is the registration-time drain, `host-future-resolve` delivers into a parked read or leaves the value.
- **Resource destructor provenance**: `host-register-resource-dtor` / `host-resource-finalizer` register per provider instance, so two providers of one `ct` keep distinct releases; a drop resolves on the handle's own instance, then the canon's instance, then the ct's owning provider via the import-linked instance graph — no `ct`-keyed global. A borrow parameter resolves through `host-resource-rep` against the dynamic caller's table, with `*host-provider-inst*` as fallback.
- **Packages**: `clocks` (`monotonic-clock` `now`/`get-resolution`, `system-clock` `now`, `types.duration`; `wait-until`/`wait-for` declared but unsupported — no timer reactor); `random` (from `/dev/urandom`, never `CL:RANDOM`, short-read-capped at 1 MiB); `cli` (environment, arguments, initial cwd, exit, the three standard streams, the `terminal-*` interfaces, the command world); `filesystem` (the capability model, the `descriptor` resource, 25 non-stream methods with the errno → `error-code` table, the four stream methods over `pread`/clamped `pwrite`/`fstat`-derived append, flag gating, `read-directory` via `fdopendir`/`readdir`); `sockets` (the `wasi-network` authority, TCP and UDP sockets, `ip-name-lookup`, the errno → `sockets-error-code` table, shared-ownership reference counting where the fd closes at zero and a stream drop is `SHUT_WR`/`SHUT_RD`).
- **Capability invariants** (§9.1), each pinned by the filesystem security suite or the sockets policy matrix: no ambient authority (everything comes from a `wasi-context`; `--dir`/`--env`/`--net`/`--listen`/`--dns` are refused without `--wasi`); handle-relative filesystem with no path-prefix filter (every operation is `*at` against a descriptor fd); symlinks never followed for the guest (`O_NOFOLLOW` walk, `readlinkat` under a 32-link budget, absolute or `..`-bearing targets refused); no cross-authority moves; non-owning preopens; network policy consulted per operation.

Measured at that point: units 481 / 0. Core and component lanes were unchanged by M16 — for their current numbers see [`conformance/`](conformance/README.md).

## M17 — close-out and removals

- **Lane close-out, measured 2026-10-30**: 0 fail × 30 consecutive runs on a quiet machine (`red_runs=0/30`, `fail_commands=0`, walls 9.86–10.53 s over 1,631 commands per run), units at 553 pass / 0 fail, and a clean checkout of HEAD building and passing both unit lanes. The pins that made that durable rather than lucky: `resume-later` on a live thread and the single-park mark credit; atomic thread-index registration with hole traps; the suspend park accepting only a spec-level resume credit; target-limited switch handoffs; reserved handoff windows (`*right-handoffs*`); a resumer's boundary being the whole `switch_to` chain rather than the arm; the per-instance sync window blocking a pending spawn's first turn; an explicit switch being a target-limited handoff whose chain ends at the target's `EXIT`; a guest keeping the store right across a nested async-callee lower; and one documented lock order whose single violating site — the store teardown — was fixed.
- **WASI HTTP removed** at user direction (`wasi:http`, M16 slice 5): out of scope, not pending work. The implementation is recoverable from git history and the vendored `…/http/*.wit` files remain as reference data only; `--http-transport` is consequently an unknown-option usage error.
- **Descriptor lifetime**: a descriptor carries a reference count (the handle plus every stream it handed out) and `closed-p`; the fd closes exactly once, at zero references, and only when the descriptor owns it. Each stream takes its own hold with an idempotent released marker. A preopen's root fd belongs to the embedder, and closing a preopen invalidates its cached descriptor so `open-at` returns `bad-descriptor` instead of reaching a reused fd number.
- **Canon-lower arms**: the position-dependent `stream`/`future` arms were deleted (probe set: 295 rows, byte-identical before and after). The `borrow` lend arms stayed: an async-typed callee resolves a borrow index at its *own* table, so the raw caller index is not substitutable — with the regressing rows recorded as `passing-resources::1` and `drop-cross-task-borrow::2/::4/::6`.

## Engine loops 18–24

- **Loop 18** — tail calls (`return_call`, `return_call_indirect`, +45 rows and 893 unsupported at that point), exception handling (tags, `try_table`, `throw`, cross-call propagation, uncaught boundary at the outermost guest frame) and the named module definition/instance script forms.
- **Loop 19** — typed function references in both backends, including `call_ref`, `return_call_ref`, `br_on_null`, `br_on_non_null` and `ref.as_non_null`; `br_on_null.wast` 10/10 and `br_on_non_null.wast` 12/12 under `--backend compiled`.
- **Loop 20** — `rec` type groups with canonicalization and iso-recursive equivalence: the wasm-comptype/rectype-group engine and the cross-module `func-type-match-p`. **Loop 21** — `struct`/`array` type declarations.
- **Loop 22** — GC values end to end: `struct.new/get/set` with packed-field normalization, `array.new/get/set/len/fill/copy/init`, `i31` with 31-bit payload semantics, `ref.cast`/`ref.test`/`br_on_cast(_fail)` with the heap lattice, `ref.eq`, and `any`↔`extern` conversions — self-authored tests, since no vendored fixture exists.
- **Loop 23** — multi-memory runtime and memory64 end to end: memidx immediates on every memory op, cross-memory copy with bounds on both backings, i64 addressing with addrtype-typed size/grow/fill/init rules, the `(memory i64 …)` text marker and limits flag `0x04`.
- **Loop 24** — SIMD: the full 236-op v128 family plus 20 relaxed ops with documented canonical choices, scalarized per [`design/PLAN.md` §7.3](design/PLAN.md), with 32 self-authored pins. The one vendored SIMD fixture, `obsolete-keywords::10`, passes at decode with the honest malformed message.

## Core-suite measurement history

- The pinned suite is 97 `.wast` files at `b0ec1f3f449525c217dbff3f1dfab435a756ea92`, run through the stateful runner with failure-phase classification: 21,187 pass / 0 fail / 73 unsupported / 0 harness-error of 21,260 — 99.7 % with unsupported in the denominator. The full run completed in ~3 s and had to stay fast; a slow or hanging run is a bug, not a workload.
- **Deliberate dip and recovery.** The pass rate fell from 95.2 % to 92.7 % on purpose: the `(module quote …)` wrap began parsing ~543 payloads that had previously "passed" without ever being parsed. It was then fully recovered and exceeded — 95.6 %, above the pre-dip 95.2 % — by fixing the exposed gaps, never by reverting the wrap.
- **Honest reclassification.** Unsupported rose from 277 to 726 and then settled: actions against genuinely out-of-scope modules classify cleanly instead of crashing as harness errors. The 73 that remain are terminal — the annotations proposal (`@…` forms), unadopted and permanently out of scope per [`design/PLAN.md` §3.3](design/PLAN.md). Every family that used to be unsupported (struct/array declarations, rec groups, typed function references, exception handling, named modules, tail calls) now passes.
- Harness limits recorded then, still true: `annotations.wast` and `inline-module.wast` are spec-test extensions outside the implemented grammar and classify as harness errors; the default linear-memory ceiling is 1,024 pages (64 MiB, [ADR-005](adr/ADR-005-memory-ceiling.md)); a 64 MiB string-literal / assembled-binary ceiling and a 1M-locals-per-function validator preflight in `src/wast.lisp` turn hostile input into catchable `decode-error`s; a failed module command leaves the script's current-instance unset, so later `invoke`/`get` fail, matching the spec harness's observable behaviour.

## Compiled-backend divergence

The compiled backend's core-suite status was measured separately, one process per
file (`--backend compiled` through the WAST runner), and was never the green
interpret baseline: 86 of the 97 pinned files were fully green and 11 diverged.

| Files | Symptom |
| --- | --- |
| `call`, `call_indirect`, `fac`, `skip-stack-guard-page` | Control-stack exhaustion: no `return_call`/`return_call_indirect`, so tail-recursive fixtures recurse on the host stack. |
| `instance`, `return_call`, `return_call_indirect`, `table_grow`, `table_size` | Partial failures around reference-types tables. |
| `type-equivalence`, `type-rec` | Partial failures. |

All eleven were pre-existing compiled-backend gaps; the same files pass on the
interpret lane. The backend did reach a component's embedded cores — the CLI
threads `WASMDOWN_API_MODE` through `run-component :mode`, so
`WASMDOWN_API_MODE=compiled make test-wasi` really compiled the guest cores —
but slowly: the 6 MB SQLite core needed a large compile-time heap and ~200 s per
component case, which is why the runner defaults to an 8 GB heap and why the
quick gate stayed the interpret lane. Disk writes under compiled code became
correct on 2026-11-01: the emitter used to push a local's *symbol* as a
`local.get` operand, so a later `local.set` changed the value behind it
(SQLite's `++data[hdrOffset+4]` landed at address 5 instead of the page buffer).
`%CAPTURE-PENDING` now copies the current value at the write whenever a pending
operand still references it, pinned by `codegen/mutable-local-operand-snapshot`.

## Upstream discovery lane

`make test-upstream` fetched the aggregated `WebAssembly/testsuite` mirror at an
exact commit and measured it through the same WAST runner: at mirror
`63908677d005` (2026-10-31) that was 65,135 commands — 61,897 pass, 3,064 fail,
101 harness-error, 73 unsupported. Never a gate: the mirror carries proposal
corpora beyond the declared scope and nothing is converted into a pass. It paid
for itself immediately, driving fixes for `v128.const` in WAST scripts; lane-wise
v128 expectations with `nan:canonical`/`nan:arithmetic` lanes; the `(either …)`
sanctioned-set wrapper; the module parser's `i8x16`/`i16x8` v128 shapes; the six
float lane comparisons (`f32x4`/`f64x2` `eq`/`ne`/`lt`/`gt`/`le`/`ge` had fed
their all-ones mask back into `f32-bits` and trapped on the first assertion); and
the load-and-extend lane count (`v128.load16x4_*`/`load32x2_*` wrote 8 lanes into
a 16-byte value instead of 4/2), plus 2 rows in the last file, `sync-streams`.

## WASI lane history

- The behaviour lane runs the real CLI, one fresh SBCL guest process per case, and treats a case that cannot run (missing fixture, timeout, harness error) as a failure, never a skip — there is no retry-to-green. Cases cover argv, exact stdout bytes (`42`, `3|13.5`, `alpha,beta,gamma`), a 20,000-line stdout through chunked stream writes, stderr staying separate with a non-zero status, on-disk persistence across two fresh processes, cross-ABI file compatibility, a seeded SQL file read through a read-only preopen, and denial. A read-only preopen cannot open a SQLite *database*: batch mode opens read-write, `-readonly` still probes the journal (the same `disk I/O error (10)`), and this shell build has no `-uri` option — the shell's contract, not a WASI defect.
- The stability run `python3 tools/run-wasi-suite.py --abi all --repeat 30 --output-dir tests/wasi-results/final --commit-summary` measured 450/450 pass over 30 fresh-process runs (preview1 120/120, preview2 330/330). The committed `conformance/wasi.md` is written only with `--commit-summary`, so a gate run cannot clobber it.
- The WASI 0.2 facade reached 27 of 31 stable interfaces bound at exact release names, and its pinned SQLite artifact *executes*: the Wasmtime-adapter component decodes, validates, composes all 17 versioned providers chosen from the component's declared imports, instantiates, and `wasi:cli/run@0.2.12` runs real SQL (`SELECT 6*7` → `42`; a CREATE/INSERT/SELECT script → `3|13.5`; with `--dir DIR::/work` the database is created, reopened by a second process, and read by the Preview1 build). Four ABI fixes made that work: variant tag matching (`%abi-case-index`), the `:enum` domain shape (`(list :enum label)`, label is `second`), and the `result<T,E>` layout, where alignment and size had to change together (`result<enum,error-code>` is alignment 1, size 2).
- Preview1 landed the complete `wasi_snapshot_preview1` linear-memory ABI — all 46 imports from the pinned WITX, with the memory gate installed before the start section, pointer/count validation in Lisp integers first (`FAULT`/`INVAL`/`OVERFLOW`), the `..`-confined path rules, `fd_readdir` cookies, and `poll_oneoff` normalizing each subscription against its own clock. `proc_raise` returns `NOTSUP` for every valid signal: the documented in-process limitation, leaving the hosting SBCL untouched.

## Backend parity program (2026-11-01)

Two explicitly selected guest backends — `:interpret` (the reference) and
`:compiled` (every defined function compiled before the start section) — were
brought to observable agreement, and the mechanism record lives here because
the ADRs are amended, not rewritten.

- **Function identity.** Compilation no longer allocates replacement
  `:host` func-instances. `func-instance` gained `backend` / `compiled-entry`;
  the entry is filled on the defined function's existing `:wasm` reference, so
  exports, tables, element segments, `ref.func` constants and other modules'
  imports keep the same object, and an imported function keeps the backend of
  its defining instance. ADR-004 carries the dated amendment note.
- **Instantiation order.** `make-module-instance` gained a `:prepare` hook
  (allocation and segment states done, active segments unapplied, no guest
  code run). `:compiled` compiles there, so a `codegen-error` precedes every
  guest-visible effect and the start section itself runs compiled.
- **One call protocol.** `runtime:invoke-wasm` owns guest depth
  (`*guest-depth*` / `*guest-depth-limit*` 1000), the tail-transfer trampoline
  (`*tail-call-tag*` + `tail-transfer`, used by `return_call`,
  `return_call_indirect`, `return_call_ref` and by a tail landing on a host
  import) and the outermost exception boundary. The interpreter's
  `interp-invoke` became a body runner (`interp-run-body`), so depth is counted
  once and the two backends cannot drift apart on frame accounting.
- **Lowering repairs.** Parallel branch moves (`psetf`), independent if/else
  arms from one saved entry state, block `tail` copies plus a `%capture-pending`
  audit over every saved view, canonical `func-type-match-p` callee checks with
  module contexts (a host callee compares against an empty module), cross-table
  `table.copy` through `runtime:table-copy2`, and the `table.grow` operand-order
  bug (ref, delta) that made `table_grow`/`table_size` diverge.
- **EH lowering.** `try_table` is a block frame plus `handler-bind`; a matching
  clause moves payload (plus the exception object for the `_ref` forms) to the
  target label with the same parallel move a branch uses. `throw` signals
  `wasmdown:wasm-exception`; `throw_ref` re-signals a non-null one. Tag objects
  are resolved at entry (the guarding module), because the handler runs while
  the thrower's frame is current.
- **Extension lowering.** 285 opcodes the interpreter implemented and the
  compiler refused: all 255 SIMD entries (now one value-level
  `simd:execute-op`, which the interpreter also calls), and the 30 GC/typed-ref
  opcodes (struct/array/i31/cast/eq/conversions) through value-level
  `wasmdown.runtime` helpers. Float SIMD lanes convert at the splat/extract/
  replace boundary (`f32x4.splat` used to pass a float to an integer lane
  writer, in BOTH backends). The formerly excluded signed sub-word load and
  float memory-boundary fixtures now pass on both engines.
- **Measured outcome.** Pinned core suite: 21,260 / 21,260 commands agree in
  outcome and phase (0 divergences, 0 compile refusals), `make report-backend-parity`.
  Component lane: 1,631 / 1,631 under `:interpret` (gate) and again under
  `:compiled` (report). WASI P1: the start section and `_start` both run through
  the selected backend (`backend-entry-count` proves it). The FULL P1
  acceptance lane passes under `:compiled` (14 checks across smoke,
  persistence, reopen, rollback, bail, no-preopen, read-only and escape;
  821 s of compile-heavy wall time), and two P2 cases are byte-identical
  across backends at ~200 s per compiled case.
- **Post-return isolation.** The lift binds `*post-return-context*` around the
  post-return call; cross-instance imports, `resource.new`/`resource.drop` and
  the async builtin family trap "cannot leave component instance", while
  `resource.rep`, `context.*` and `backpressure.*` stay callable. The vendored
  `values/post-return.wast` moved from 62 pass / 9 fail to **71 / 71**: the
  remaining two commands call `context.get`/`context.set` from a post-return,
  so the sync lift now hands the per-call task/thread to the post-return call
  (the same storage the body wrote). With the file green it left
  `tests/component/nyi.txt`, growing the component lane from 1,631 to 1,702
  commands (gate, compiled report and census pin all moved together).
- **The extension work also moved the interpreter lane.** Re-running the
  aggregated mirror (pin `63908677d005`) after the SIMD/GC/float-lane fixes
  reads 62,156 pass / 2,821 fail / 85 harness-error / 73 unsupported of 65,135
  commands, against the 3,064 / 101 recorded at the M17 close: the shared
  value-level semantics removed real failures, not just backend differences.
- **The released WASI lanes run in both backends.** The full P2 behaviour lane
  (11 cases) and the full P1 acceptance lane pass with compiled guest cores;
  each case asserts the same exact stdout/status as the interpret run, so the
  two backends satisfy identical expectations rather than merely agreeing with
  each other.
- **Cost picture** (`make report-backends-bench`, medians): on the synthetic
  kernels `:compiled` runs 7–17x faster than `:interpret` (fib 9.4 ms vs 91 ms,
  memory fill 159 ms vs 671 ms), and eager compilation costs 0.4–1.5 ms and
  0.3–1.3 MiB per small module. A 6 MB guest (SQLite) needs ~8 GiB of SBCL
  heap and ~200 s to compile, which is why the WASI lane's compiled runs are
  report-mode evidence rather than a gate.

## The compiled backend's name (2026-11-01)

The conservative compile tier was called `:baseline`, following PLAN §6.3's
vocabulary where it is the baseline an optimizing tier would be compared
against. In the implementation that name collided with two other meanings —
the *conformance baseline* lane kind (a measurement that is read rather than
gated) and the reference interpreter as "the baseline" — so the backend is now
`:compiled` (`WASMDOWN_API_MODE=compiled`, `func-instance-backend` =
`:compiled`, lane artifacts `component-compiled*`). Nothing about the code
path changed: still eager, whole-module, conservative SBCL `COMPILE` with
`compiled-entry` on the defined func-instances and no interpreter fallback.
PLAN §6.3 carries a matching note; the plan's `:baseline` row names the same
slot.

Re-measured under the new name: P1 acceptance all cases pass with
`WASMDOWN_API_MODE=compiled`, P2 behaviour lane 11/11 (fresh output directory
`tests/wasi-results/compiled-preview2/`), P3 command 1/1, component lane
1,702/0 — so the renamed value drives the same code path end to end.

## WASI 0.3.2 guest fixture (2026-11-01)

No toolchain in this environment emits the `@0.3.2` component ABI, so the P3
guest lane pins a **hand-authored** command: `tools/build-p3-fixtures.lisp`
assembles a component from the pinned binary grammar (header, type section with
`result<_,_>` and its functype, a core module, core instance, alias, `canon
lift`, an instance carrying `run`, and the `wasi:cli/run@0.3.2` export),
verifies it through this project's own decoder/validator/runner, and records
`P3-CHECKSUMS`. `make test-wasi-preview3` runs it through the real CLI in both
backends (exit 0); `wasi/command-component-runs-the-selected-backend` proves
the nested core ran through the selected backend via the entry counters.

The four-instruction stdout command in `tests/wasi.lisp` writes "hi"
through a `stream<u8>` and `task.return`s; it is green through
`run-component` without a reactor but is not resume-safe (it writes
unconditionally, so a reactor-driven resume re-emits `hi`). That is a
property of that fixture, not of the scheduler. The CLI lane now pins the
resume-safe replacements instead: `p3/async-callback` (state in memory) and
`p3/stream-stdout` (park the read, stage the write, wait on real events,
print exactly `hi\n`).

## P2 stream readiness and P3 importing guests (2026-11-01)

- **P2 (R1).** Against the pinned `io.wit` / `sockets.wit` for 0.2.12: `read(0)`
  and an OPEN source with no promptly available bytes now return `ok([])`
  instead of `closed`; `EAGAIN` maps to an empty read rather than a fake I/O
  error; `write`/`write-zeroes` trap when the offer exceeds the `check-write`
  permit, and a partial host write retains the unwritten suffix (200 KB into a
  nonblocking pipe: 65,536 delivered before, 200,000 after, `pending` 134,464);
  `send` enforces `check-send` and reports the sent count. Five regressions
  fail on the pre-change source and pass after; unit tests 667 -> 672.
- **P3 (R2).** The hand-authored lane gained a provider-importing command:
  it imports `wasi:cli/exit@0.3.2`, lowers `exit-with-code` into its core, calls
  it with 7, and the command runner maps the WASI exit to the process status
  (CLI exit 7). A third guest imports `wasi:clocks/monotonic-clock@0.3.2`,
  calls `now()` twice and traps unless the second call is not earlier — the
  first P3 provider call that is not an exit. A fourth guest is the first
  RESUME-SAFE binary async command: `run` is lifted async with a callback,
  the core run returns `YIELD`, the callback runs on the resume, calls
  `task.return(ok)` and returns `EXIT`; the runner reports status 0 and no
  leaked threads. A fifth guest is the stdout STREAM shape: it imports
  `wasi:cli/stdout@0.3.2`, parks an async `future.read` first, stages a
  `stream.write`, waits for the write's own event when the worker-backed
  stdout returns WOULD-BLOCK, drops the writable end (the provider resolves
  the future on that drop) and waits for the future event; the CLI prints
  exactly `hi\n`. Building it surfaced two runtime defects, both fixed:
  `%host-end-retry` cleared the parked host copy before the retry (the retry
  could not match it, advanced the generation and resubmitted the span, so
  stdout duplicated — the copy is now reused), and `%end-notify` woke only a
  sync waiter, never a `waitable-set` waiter (a task parked on the set never
  learned a copy completed — both are now woken). A sixth guest exercises
  CANCELLATION: it lowers the clock's async `wait-for` (the `async` canon
  option is required; the non-async lower flattens to `(i64)->()` and the
  module-import signature check rejects it), cancels the pending subtask
  synchronously (asserting a cancelled state) and drops it; a 1 s wait
  finishing in ~0.2 s proves the cancel ran. That surfaced one more defect:
  the async-lower pack took the callee task's mutex even when the callee is
  HOST-PENDING and has no task record (NIL `ASYNC-TASK`); the host-pending
  arm now packs without the mutex. A seventh guest is a TCP SOCKET command: create/connect/send "ping"
  over loopback, receive and wait for the echo, then drop. It is authored in
  TEXT and encoded by the new tools/component-encode.lisp (no binary
  serializer existed; six fixtures had been hand-assembled). The lane runs
  seven guests, 3 repeats each (21/21 per backend), with a suite-side echo
  server and --net for the socket case.
  *(Superseded 2026-11-02: network communication became out of scope; the
  socket guest and its fixture were removed and the lane dropped to six
  guests. It returned to seven the same day when the random guest landed
  (see “Stability gate and the seven-guest P3 lane”). Kept as history, not as
  a current support claim.)* The component binary grammar details
  that mattered: type indices
  inside an instance type are local to that type's declarations, the lower's
  core func must be defined before the bag that exports it, and imported
  instances occupy the instance index space before defined ones.

## Roadmap-era notes

- Numbering convention: engine-slice loops 18–24 and Component Model milestones M7–M17 ran in parallel; neither series renumbers the other.
- Remaining work named at the M17 close, none of it inside the lane: provider step 5/6 (DNS and blocking-stdio bounded executors, and socket accept/recv/send registering a reactor watch instead of reporting would-block to a caller that then waits); step 1's last item (the two canon-lower wrappers' positional stream/future/borrow arms, where deletion needs the verification-set fixtures in the probe set first — `%abi-load`'s memory-path transfer arms stay out as a measured negative); borrow-side lifetime pins (a live borrow held across `task.return` must trap; borrow-in-awaitable-set), reusing the guest-drop pin's shape; and one parked delta with a measured verdict, `wip/type-view` (at parity in a clean window, not landed because a green lane's bar is "no measured cost"). `wip/reactor-wiring` was closed as superseded: its driver pump and context reactor landed as the store driver, its socket plumbing as the executor slice with a strictly stronger keying scheme, so it was never rebased or re-measured.
- Residual security/ABI honesty items recorded with M16: no socket or timer reactor, so a guest cannot be woken later and `clocks.wait-until`/`wait-for` stay declared-unsupported; the nested `option<stream<u8>>` through a host lower (resolved 2026-11-02 — see “Nested async payload ownership” at the end of this file); descriptor-destructor and lend-list accumulation for long-lived hosts (resolved 2026-11-02 — see “Host borrow reclamation and store retirement” at the end of this file); `%abi-lower`'s top-level error-result discriminant for compound outer shapes (audited and withdrawn 2026-11-02 — see “Compound error-result audit” at the end of this file); pre-existing text/validator limits around imported-instance functypes with outer compound refs (the runtime lifts with the real WIT functype, so fixtures work around the validator, not the ABI); `@unstable` gating parsed but not enforced (the timezone interface is simply not registered; enforced 2026-11-02 — see “Stability gate and the seven-guest P3 lane”); `wasi:sockets` has no `shutdown` in this WIT revision; and world `include`, `implements`/`external-id` and nominal resource typing are not read by the WIT frontend.
- Items open since the M7-era measurement: the compiler-backend parity matrix (§12.3's `:optimized`/`:aot` plus the differential oracle), the property/fuzz lanes (§12.6), and the remaining CI lanes (M11). Threads/atomics and the FASL cache were never started.

## Network communication excluded (2026-11-02)

By user decision, **network communication — TCP, UDP and DNS — is permanently
out of scope**, together with the CLI network options `--net`, `--listen` and
`--dns`. This is not an implementation gap and not planned: networking is
removed from the declared WASI profile, from the P3 guest lane, from the
conformance matrix's support rows and from the milestone work lists (F2 no
longer names sockets).

What changed in the tree:

- A component that **declares** a `wasi:sockets` import (any interface, any
  version) is refused at link time, before any guest code runs, with this
  exact message:

  ```
  Network communication (TCP, UDP and DNS) is not supported by this design; there is no wasi:sockets provider for guests. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.
  ```

- The CLI refuses `--net`, `--listen` and `--dns` as usage errors (exit 2)
  before building any authority; no network authority exists in the CLI.
- The seventh P3 CLI guest — the TCP loopback socket command, previously run
  against a suite-side echo server with `--net` — was removed. The P3
  hand-authored lane dropped to **six** guests (18/18 ×3 per backend); later
  the same day it returned to seven when the random guest landed (see
  “Stability gate and the seven-guest P3 lane” below).
- The 0.3.2 `wasi:sockets` provider remains in the runtime **only** because
  the pinned `wasi:cli/command` world imports
  `wasi:sockets/types@0.3.2` and `wasi:sockets/ip-name-lookup@0.3.2` itself;
  that internal composition is an implementation detail, not a guest surface
  or support evidence. The vendored
  `specs/wasi/wasi-wit-0.3.2/sockets/sockets.wit` is reference data only.
- `wasi:http` remains excluded (removed in M17) under the same policy; both
  exclusions are enumerated in `specs/wasi/MANIFEST.md`.

The declared profile is therefore `conformant for the declared WASI 0.3.2
profile (wasi:http and wasi:sockets excluded)` at most, and `partial` until
the §2.1 conditions hold. The canonical network message above is the only
refusal text for this surface; the canonical HTTP message is unchanged.

## Stability gate and the seven-guest P3 lane (2026-11-02)

Two §9.3 rows closed in the same change: `@unstable` is now **enforced**, and
the P3 CLI lane gained a **random** guest, taking it to seven hand-authored
@0.3.2 commands (21/21 ×3, both backends).

**`@unstable` refusal.** The 0.3 composition path checks every `wasi:` import
a component *declares* before anything is composed. It does not re-read
annotations: it delegates to the WIT selection surface
(`wit-import-surface-status`), which already answers “stable / unstable
feature / other reason” from the pinned package set and the enabled feature
set. A declared import whose target interface is `@unstable(feature = …)` and
whose feature the selected surface does not enable is a `link-error` naming
the import and the feature:

```
wasi:clocks/timezone@0.3.2 is @unstable(feature = clocks-timezone) and that feature is not enabled; unstable interfaces are outside the selected stable WASI 0.3.2 surface (see docs/usage/wasi.md and specs/wasi/MANIFEST.md).
```

Stable and unknown names pass through untouched, so the 0.3 command world
composes exactly as before; the 0.2 line is untouched. **No feature flag was
added** — the CLI has no way to enable a feature, so the gate is
refusal-only. Ordering is deliberate: the unstable check runs before the
sockets-import refusal. On the 0.3-only path both see every declared
`wasi:` import; on a mixed component they run over the 0.3-line subset only
(the 0.2 line stays with the P2 tables), so neither path can hand a gated
import a provider. Pins:
`wasi/component-unstable-declared-import-is-link-error`,
`wit/selection-import-surface-query` and
`wit/selection-command-world-timezone-import`.

**Random guest.** `p3/random-bytes` (`p3-random-guest.wasm`, authored in TEXT
and encoded by `tools/component-encode.lisp`, with the builder self-checking
it) imports `wasi:random/random@0.3.2` plus `wasi:random/insecure@0.3.2` and
`wasi:random/insecure-seed@0.3.2`. It exits 0 only when two 16-byte secure
reads have exact lengths and differ and the `u64`/seed calls complete; any
failure traps to a nonzero exit. The list-returning functions exercise the
memory+`realloc` lowering path. The lane remains seven guests — smoke, exit
importer, monotonic-clock `now`, resume-safe async callback, stdout
`write-via-stream`, cancelled subtask, and random — at 21/21 ×3 in both
backends. The socket command stays removed: network communication is excluded
and is not lane coverage.

**Encoder fix.** `tools/component-encode.lisp` grew the inline-export path for
component instances: the decoder keeps an export name as `(name attrs)`
(Binary.md nameattributes), and the encoder now unwraps that cell and re-emits
the `0x00` attrs prefix instead of encoding a cons as a string. This was the
previously unexercised instance-`run` shape.

Evidence at the change: unit tests 685/0, `make verify` green, canonical
`tests/wasi-results/p3-*` re-recorded.

## Deadline liveness and the cli/stderr/terminal guest — nine-guest P3 lane (2026-11-02)

Two §9.3 evidence rows closed and the lane grew to **nine** hand-authored
@0.3.2 commands (27/27 ×3 in both backends), superseding the seven-guest
count above.

**Liveness oracle.** The async lift's drain/replay loop declared a store
quiescent after a fixed budget of roughly 100 ms with no *ready* task. That is
wrong when a guest is parked in `wait-for`: the reactor's timer is about to
fire, yet the store looks idle, so the oracle reported a deadlock and the
guest never observed its deadline. The fix makes the liveness query name the
one thing that guarantees future progress: **pending host work plus an armed
one-shot reactor deadline**. The generic predicate is
`%host-store-future-progress-p` on the store; the WASI specialization is the
`%reactor-pending-deadline-p` generic, implemented by the reactor as the
exported `reactor-pending-deadline-p`, which is true only while a live
one-shot deadline registration is armed (a spent or cancelled registration and
an empty reactor are false). A bare fd-readiness watch is deliberately
**excluded**: a peer may never become ready, so a readiness watch alone is not
a liveness guarantee. Genuine deadlocks still trap — `component/deadlock.wast`
and `dont-block-start` remain oracle-tripping fixtures in the component async
gate (1,702/0). Unit pin: `wasi/reactor-pending-deadline-p` (unarmed reactor;
fd watch only; armed deadline; store work-count gate through
`%HOST-STORE-WORK-ADD`; cancellation removes it; a fired one-shot leaves
nothing behind).

**Deadline guest as the regression proof.** `p3-deadline-guest.wasm` lowers
the clock's async `wait-for` with a **200 ms** request — deliberately past the
old ~100 ms budget — joins the subtask into a waitable set and returns
`WAIT`; the reactor's callback loop parks until the deadline fires, and the
resumed callback traps unless the subtask returned after at least 200 ms of
clock time (`t1 - t0 >= 200 ms`), so an early completion fails. It passes in
both backends, three repeats.

**cli/env/stderr/terminal guest.** `p3-cli-env-stderr-terminal`
(`p3-cli-guest.wasm`) imports `wasi:cli/environment`, `wasi:cli/stderr` and
`wasi:cli/terminal-stdout` @0.3.2. It reads `get-arguments` and
`get-environment` as `list<string>` / `list<tuple<string,string>>` and
`get-initial-cwd` / `get-terminal-stdout` as options through
`realloc`-lowered ABI layouts, and traps unless argv[0] is the fixture path
the CLI supplied, argv[1] is `wmd-cli-marker`, the environment carries
`WMD_ENV_PROBE=env-probe-value`, the initial cwd is an option and
`terminal-stdout` is `none` on the captured (non-tty) stdout. It then writes
`p3-cli-guest: stderr probe` through stderr's `write-via-stream` in the
canonical future order (park the future read, stage the write, drop the
writable end, wait for the future event); stdout stays empty and the exit is 0.
The suite runner gained per-case `--env` plumbing, with a `{fixture}`
placeholder expanding to the path handed to the guest as argv[0].

**Lane and evidence.** Nine guests — smoke, exit importer, monotonic-clock
`now`, resume-safe async callback, stdout `write-via-stream`, cancelled
subtask, random, deadline-wait, cli-env-stderr-terminal — at 27/27 ×3 in both
backends (`P3-CHECKSUMS` and `tests/wasi-results/p3-*` re-recorded). Unit tests
686/0, `make verify` green, component async gate 1,702/0 with the deadlock
fixtures still trapping.

## Filesystem and stdin guests — ten-guest P3 lane (2026-11-02)

The §9.3 filesystem evidence row closed and the lane grew to **ten**
hand-authored @0.3.2 commands (30/30 ×3 in both backends), superseding the
nine-guest count above.

**Filesystem guest.** `p3/filesystem-preopens-read` (`p3-fs-guest.wasm`, TEXT
encoded) imports `wasi:filesystem/{types,preopens}@0.3.2`. With a `/work`
preopen seeded with `hello.txt` (`filesystem-probe\n`), the guest takes the
preopen handle from `get-directories`, issues an async `open-at` and waits for
its SUBTASK/RETURNED event in a waitable set, reads exactly 17 bytes through
`read-via-stream` + `stream.read` (a blocked copy is waited with the packed
`CopyResult|progress<<4` payload), and confirms the paired future is ok. A
second `open-at` of `missing.txt` must return an error result with
`error-code::no-entry` (case 19) — an err value, not a trap. Every handle
(opened descriptor, both stream ends, the preopen handle) is dropped; any
mismatch traps, so exit 0 is the assertion.

**Stdin probe.** The cli guest now also imports `wasi:cli/stdin@0.3.2`: it
reads the suite's real stdin bytes (`stdin-probe\n`) through
`read-via-stream`, requires the explicit DROPPED EOF result and an ok paired
future, and traps on any other byte. The runner supplies the marker from the
case manifest. The cli/stdio/env/terminal row therefore covers stdin,
environment, stderr and terminal and stays resolved.

**Bare-label variant guard.** `%abi-store-variant` accepted a bare label for
any variant case, so a payload-bearing case would store only the discriminant
and leave the payload slot stale. It now raises `wasmdown:host-error`
(`a bare variant label (...) names a case with a payload; pass (label .
payload)`) for that shape; payload-less bare labels remain the
native-provider shorthand and the tagged form is unchanged. Pinned by
`component/abi-bare-variant-label`. This guard does not touch descriptor
dtor/lend reclamation; that defect was resolved later the same day — see
“Host borrow reclamation and store retirement” at the end of this file.

**Lane and evidence.** Ten guests — smoke, exit importer, monotonic-clock
`now`, resume-safe async callback, stdout `write-via-stream`, cancelled
subtask, random, deadline-wait, cli stdin/env/stderr/terminal, filesystem
preopens/read — at 30/30 ×3 in both backends (`P3-CHECKSUMS` and
`tests/wasi-results/p3-*` re-recorded). Unit tests 687/0 (688 after the
nested-async fix below), `make verify` green,
component async gate 1,702/0 with the deadlock fixtures still trapping.

## Nested async payload ownership — load arm, guards and type space (2026-11-02)

**Measured defect (now closed).** A `stream`/`future` payload read out of guest memory by the
generic `%abi-load` catch-all returned the raw handle index and left the
readable end resident in the loading instance's table — so a nested payload
never crossed a boundary and ownership stayed with the loader by accident. On
the async result side, a nested `(:idx . n)` resolved against the **caller's**
type space, so an `option<stream<u8>>` result could name the provider's type
but be read as whatever shared that index in the caller.

**Load arm.** `%abi-load` now implements load's `StreamType`/`FutureType` rows
(§2133–2169) by delegating to `%lift-async-end` (§2161 → `lift_async_value`,
§2438): the memory slot indexes the **loading (caller) instance's** handle
table, the readable end is removed from that table, its owner and index are
cleared (§2446), and the source slot is freed for reuse. A second load of the
same index is therefore a stale index — the slot no longer holds an end — and
traps.

**Guard set.** `%lift-async-end` gained `lift_async_value`'s checks. The end's
recorded element `ct` must match the valtype's element through
`%abi-type-key`, which resolves both sides into one type space (struct `ct`s
compare by identity; a prim element is a keyword on the end but a fresh prim
`ct` on the valtype side; a raw `(:idx . n)` resolves through the given
space). The readable class must match the valtype, and own/borrow compare
class only because resource indices are alias-projected. The lift clears
owner+index on the value it returns.

**Type space.** The async result-store context now carries `:type-inst`, the
provider (callee) type space, so a nested `(:idx . n)` in an
`option<stream<u8>>` result resolves there rather than against the caller's
types. The pin measures this with a colliding `u32` at caller index 0: a
caller-space resolution would store an integer instead of the end record.

**Pins and negative controls.**
`component/generic-path-five-cases-pin` CASE 2 was migrated from the frozen
catch-all's expectation (raw index, end still resident) to the ABI contract:
the loaded value is the end record, the source slot is null, freed for reuse,
and the second load of the same index traps. A stale expectation was exactly
the leak the old measurement recorded. The new
`component/indirect-async-payload-ownership-pin` covers (a) a real sync host
lower with the tuple forced into memory past `MAX_FLAT_PARAMS` — 16 `u64`
params plus the option flatten to 18 slots — where the host provider sees the
end record nested in the option, never an integer, the caller's source slot is
consumed, and the provider's table never sees the end; (b) async
`option<stream<u8>>` results with colliding type indices, including the
`none` case allocating nothing and leaving both handle tables unchanged; and
(c) `lift_async_value`'s trap set through load — stale index, a resource
handle in an end slot, a stream end through `future<u8>`, a wrong element type
(`stream<u32>` end for a `stream<u8>` valtype), a writable end, a non-idle
end, and an end in a waitable set — with the final readable, idle,
out-of-set end still lifting, so the guards do not consume the slot. Negative
controls confirmed both the load arm and the guard set are load-bearing.

**Lane.** Unit 688/0 (687 before), component 1,702/0, P3 10/10 ×3 in both
backends (30/30) unchanged; `make verify` green.

## Host borrow reclamation and store retirement (2026-11-02)

**Measured defect.** A canonical lower stages the borrow handles it added to
the callee's Table in `*pending-borrow-handles*`, for the callee Task that
must adopt them (§2832 → §2838). A **host** callee has no Task: the call
bound and cleared the dynamic list and simply dropped the records, so the
borrow handle stayed in the table with no owner and nothing ever removed it.
`%handle-add` was append-only by design (fixtures pinned absolute indices), so
every host call with a borrow parameter grew the table by one slot — the
"lend lists accumulate" half of the old Known-gaps bullet. The destructor
half was the same shape: `resource-dtor-table`, `ct-defining-inst` and
`comp-closure` key on, and retain, the graph's comp-instances, and nothing
retired them when the store was discarded.

**Reclaim policy.** Each staged entry is now a struct `PENDING-BORROW`
carrying the **owner table**, the 1-based **slot** and the **handle**. A
host-callee call reclaims its staged records exactly once:

- on the immediate return and on a trap, via `unwind-protect`;
- across a HOST-PENDING, they are **retained** for the pending's whole life
  and removed in the continuation's own `unwind-protect` at the **terminal**
  outcome — resolve, reject, or an **acknowledged** cancel;
- a cancel **request** is not terminal (the provider may not be able to
  stop), so the borrow stays live and a later resolve still reclaims it;
- removal clears the slot and returns it to the free list only while the slot
  still holds **the same record**, so a slot already reused by another class
  is never disturbed.

The source handle's `num_lends` ledger is untouched by this cleanup: it
belongs to the Subtask and is cleared by `%subtask-deliver-resolve`, so the
loan stays counted until delivery.

**Shared free-slot policy.** `%handle-add` now delegates to the one Table
allocator (`%handle-add-any`) and therefore reuses the instance's free list
(§1131) instead of appending. The slot's former occupant is gone; a stale
index that names another record class is rejected by the callers' class
guards, so the reuse is not type-safe by construction.

**Store retirement.** `%store-teardown` now clears `comp-closure`,
`resource-dtor-table` and `ct-defining-inst`. The safety argument is a
lifecycle proof, not a heuristic: every store-lifecycle caller discards the
store after the call — the wast run hook's per-script store,
`call-with-component-store`'s close path, `run-component`'s own store and the
pins' fresh stores — so nothing can look a destructor or provenance record up
afterwards. Teardown retires the metadata of the store it is **given** as well
as the bound store (the named-store retarget: an explicit argument must still
retire the store it names), while the scheduler-state teardown (thread
registry, execution right/ticket, handoff frames, driver records, tick gates)
still runs on the **bound** store. A **live** graph's store is never touched:
teardown is per store, and dropping one handle does not remove the type's
destructor while another handle of the same type is live.

**Plateau evidence.**
`component/store-metadata-retires-with-store-pin` runs a warm-up cycle and
then four repeated `instantiate → new/drop → teardown` cycles: the live
metadata counts are identical every time — `(1 2 0)` for dtor/provenance/
closure, never iteration-shaped — and `(0 0 0)` after each teardown. Inside
one graph, two owning handles of the same type occupy slots 1 and 2; six
destructor runs (two drops plus four new/drop cycles) leave the handle vector
at **capacity 2** and the free list at **2**, so the table and free list
plateau; the first drop keeps the type's dtor registered because the second
handle is live.
`component/resource-metadata-live-graph-isolation-pin` holds two live graphs
in two stores: dropping in A runs A's destructor and leaves both metadata
sets intact, tearing down B cannot strip A's provenance, and each store
retires only its own records. `wasi/fs-descriptor-drop-fd-plateau-pin`
covers the descriptor side: six `open → stream → drop → stream-end` cycles
keep the process open-fd count at baseline, a stream that outlives its handle
keeps the fd until the last release, and the last release closes it exactly
once. No heap-size assertion; the fd count and the structural counts are the
observables.

**Pins and migration.** `component/host-borrow-reclaim-pin` (immediate
return, trap, a nested guest call does not inherit the account, free-slot
reuse, source handle untouched, source `num_lends` still the subtask's),
`component/host-borrow-retained-across-pending-pin` (retained through
resolve and acknowledged cancel; a cancel request alone is not terminal; a
late resolve still reclaims), plus the two metadata pins above.
`component/generic-path-five-cases-pin` CASE 3c/B was migrated: it used to
assert the leaked slot was still resident, and now asserts the slot is empty
and the index is back on the free list while the source index still owns its
resource. The pins carry both positive and cleanup assertions (free-list
membership, post-teardown `(0 0 0)`).

**Residual risks.** Reclaim is store-retirement-based, so a caller-owned
store reused for many top-level instantiations **without** teardown still
accumulates dtor/provenance rows — there is no per-instantiation pruning, and
a live graph must keep its dtors resolvable. Scheduler-state teardown still
targets the bound store. Outstanding non-terminal host pendings are not
force-reclaimed at teardown. A freed slot can be reused by any record class,
with the class guards (traps) as the only type check.

**Lane.** Unit 693/0, component 1,702/0, P3 10/10 ×3 in both backends;
`make verify` green.

## Compound error-result audit — residual withdrawn (2026-11-02)

The known-gaps bullet that said `%abi-lower` could lose the top-level
error-result discriminant for **compound outer shapes** was audited and
withdrawn: there is **no reproducer** against this tree. The `:result` arm
lowers to `variant("ok", "error")`, whose join always leads with the
discriminant, and the existing pin already covered nested `result`, record,
list and primitive arms. `component/abi-compound-result-lowering` was
extended with four more shapes — both arms empty records (`(0)`/`(1)`),
mixed-arity tuples, a variant carrying a record, and an `option`/`option`
pair — all passing unchanged. **No lowering code changed.** The per-case
validator limits around imported-instance functypes with outer compound refs
were never an ABI-lowering issue and remain open in their own row. Recorded
as an audit outcome, not a fix.

## Filesystem write path — offsets, append, stat (2026-11-02)

The P3 filesystem guest (`p3-fs-guest.wasm`, same suite case) grew the write
half of the row: open-at with create|truncate, `write-via-stream` at offset 0,
a second write at offset 17 (the 2-byte gap must be zero-filled),
`append-via-stream` at EOF, the BLOCKED event path through a waitable set,
parked write futures that must resolve `result::ok`, and `descriptor.stat`
(`ok`, regular-file case 5, link-count 1, size 22; the payload sits at
retptr+8 because the variant join alignment is 8 from the u64 fields). A
same-descriptor `read-via-stream` reads all 22 bytes and compares them
byte-exactly; the suite adds a host-side `expect.file_contents` witness
(`wmd-write-probe\0\0!-app`). The missing-path probe was also fixed to rewrite
the full open-at tuple (its leftover create|truncate flags could have made it
pass vacuously).

The row stays partial: directory operations, write-side error paths
(read-only descriptor/preopen), stat timestamps and the advanced calls
(`set-times`, `set-size`, `sync`, `advise`, `metadata-hash`, `get-flags`,
`get-type`, `stat-at`, `is-same-object`) are provider/unit-tested only; 0.3.2
has no seek API, offsets are explicit stream arguments. P3 30/30 ×3 both
backends, `make verify` green, only the filesystem fixture hash changed.

## Validator type-use owner-space resolution (2026-11-02)

The last §9.3 defect row closed. An imported-instance functype whose
parameter or result is a **compound** type brought in by an outer alias was
materialized against the wrong type space: `entry-at` found the outer
compound in the owner, then re-resolved its nested `(:idx . k)` against the
instance **declarator** space, where an unrelated same-numbered type could
win. The collision is exact in the pin: owner types `0 = u32`, `1 =
tuple<u32>`, declarator types `0 = string` (decoy), `1 = outer alias to
owner 1`. A `param x :idx 1` therefore either demanded memory that
`tuple<u32>` does not need, or missed the requirement a real `string` leaf
does.

Fix: `entry-at` now returns `(ct, owner-view)`; an outer alias switches to
the owner selected by its de Bruijn count, and an eq-bound export propagates
the target's owner. A ct taken from an outer owner is materialized against
that owner's space by `%materialize-component-ct`, owner-qualified through
nested outer aliases and eq bounds, while declarator-local entries keep the
declarator remap.

Pinned by `component/remap-outer-compound-functype-owner`: the direct
remap in both directions; the exact end-to-end diagnostic pair (the
decoy-first text validates with no memory, while the real-`string` text
still rejects with a message naming `memory`); and result-position
companions of both. The pin is negative-controlled by construction: the
decoy-first positive fails under the old remap, and the string negative
fails if the owner switch silently resolves everything to the decoy.

Unit 694/0, component 1,702/0, P3 30/30 ×3 both backends, `make verify`
green. This closes the validator-limit row; no §9.3 defect row remains
open.

## Filesystem directory/error guest; validator walk and s33 encoder fixes (2026-11-02)

The P3 filesystem guest (`p3-fs-guest.wasm`, same suite case
`p3/filesystem-preopens-read`) grew the directory and error surfaces. The
WIT-shaped instance type the guest needs then exposed two engine defects that
the earlier fixture could not reach.

**Directory and error guest.** On the `/work` preopen the guest now:
`create-directory-at` a `subdir`; `open-at` it as a directory; `stat` it as
`directory` (case 2); `read-directory` on the preopen and require the
enumeration to name `hello.txt`, `out.txt`, `subdir` and `temp.txt` with the
paired future ok; `rename-at` `temp.txt` to `moved.txt` and `stat-at` the
result as regular-file with size 0; `unlink-file-at` it; `remove-directory-at`
the `subdir`; and then require `stat-at(subdir)` to return err
`error-code::no-entry`, never a trap. On the error side, a
`write-via-stream` through the read-only `hello.txt` descriptor must resolve
err `error-code::read-only` (case 32) through the paired future, again never a
trap. The suite adds an `absent` witness (`subdir`, `temp.txt`, `moved.txt`),
so the cleanup is host-observed, and the guest removes every temporary object
so the three repeats share the workdir. The builder's scratch dirs use
pid+counter rather than the random state (SBCL's default `random-state` is
deterministic), which makes rebuilds idempotent; `P3-CHECKSUMS` was re-pinned
to `a3293778…` for the new binary.

**Validator instance type-index space.** `%vis-check-type-use` pushed every
import/export declarator inside an INSTANCE type, but Binary.md 274 says only
type-bound declarators introduce a TYPE index; an INSTANCE type's imports and
func/method exports do not. A COMPONENT type keeps every declarator
(Binary.md 415), and that arm is unchanged. The old walk therefore shifted
later numeric references onto a same-numbered wrong declarator. The earlier
fixture passed because its references happened to land on slots the validator
accepted: it referenced its local `descriptor-stat` definition, not the
exported slot, from the later method declarators. The new guest references the
exported `descriptor-stat` and `directory-entry` slots from methods (and
`read-directory`'s `stream<directory-entry>` element must resolve exactly),
which makes the shift observable. The walk now pushes only
`%type-indexing-declarator-p` declarators for instances.

**Encoder s33 valtype indices.** `tools/component-encode.lisp` wrote component
valtype type indices with `u32`. Binary.md 252 requires signed s33: index 64
must encode as `C0 00`, but the old writer emitted `40`, which is the FUNC
constructor byte, so a component with a valtype reference at index 64
round-tripped malformed and the decoder could not read it back as a type
index. The new fixture's type space reaches that index, so the round-trip
guard caught it; the earlier fixture's indices stayed below 64, where the
`u32` and s33 bytes coincide. A `s33` helper now drives every valtype `:idx`.

Neither fix was filed as a §9.3 defect row (the open row at the time was the
filesystem evidence row), but both are real correctness fixes with the P3
filesystem fixture/lane as the regression witness; no separate named pin
exists for them. Unit 694/0, component 1,702/0, P3 30/30 ×3 both backends,
`make verify` green; only the filesystem fixture hash changed.

## Deterministic async property lane and executor audit (2026-11-02)

F4's async-assurance item landed as a real gate rather than more manual
probes. `tests/async-property.lisp` (runner `tools/run-async-property.lisp`,
target `make test-async-property`) drives the runtime's own primitives with
fixed xorshift64* seeds `1,2,3,5,8,13,21,34` x3 repeats (14,071 checks, 0
failures; `ASYNC_SEEDS`/`ASYNC_REPEATS` override the corpus). The random walk
interleaves terminalise/cancel/ack/continuation/pump for `host-pending`,
resolve/drop/cancel/pump for the one-shot `host-future-watch`, and
stage/copy/poll/cancel/drop for stream/future ends; deterministic arms cover
`task.return`/`task.cancel`, subtask resolve/deliver/drop, lend return,
armed-event preservation under cancel, stale `host-end-ready` generations, and
the host sink/source parked copy. A scripted fake reactor consumes one thunk
per `%store-tick`, and the lane asserts the idle gate, the 1 ms budget clamp
and the deadline-readiness oracle. Worker synchronisation is
semaphore/condition based; every bounded wait that expires is a failure, and
no test sleeps.

The checks are invariants, not golden outputs: terminal outcome first-wins,
exactly-once continuation/event/delivery, no double resolve, monotone
task/subtask/end/watch states, the store-work unit released exactly once,
buffer/progress and copy-generation monotonicity, backpressure admission and
exclusive-slot serialisation. `make test-async-property-negative` deliberately
double-delivers a pending continuation and requires the same checkers to
report it (negative control). A wider 40-seed x4 sweep (92,358 checks) was
green.

The executor audit used the same lane plus the existing `tests/wasi.lisp` pins
(`wasi/host-executor-budget-and-close`, `...-close-never-joins`,
`...-separate-budgets`, `wasi/stdio-blocking-{sink,source}`,
`wasi/dns-async-pending-and-detach`): DNS and blocked stdio are separate
`host-executor` services with fixed construction-time budgets (2 workers each,
never grown), jobs run only on the service's named worker threads, active jobs
never exceed the budget, a blocked fd's span is copied into a worker-owned
vector, `%executor-close` drops queued jobs/refuses new submissions without
joining an in-flight job, and `close-wasi-context` marks the context closed so
`%context-work-gate` rejects new DNS/stdio work and detaches both services.
Guest stdio is the P3 `p3/cli-stdin-env-stderr-terminal` and
`p3/stream-stdout` evidence: 30/30 x3 in both backends. Unit 695/0, component
1,702/0, `make verify` green.

## Upstream `wasi-testsuite` adapter lane — green (2026-11-02)

`adapters/wasmdown.py` now implements the upstream runtime-adapter contract
(`get_name`/`get_version`/`get_wasi_versions`/`get_wasi_worlds`/
`get_timeout_seconds`/`compute_argv`) for `wasm32-wasip1` + `wasm32-wasip3`,
`wasi:cli/command` only. Each guest is one fresh
`sbcl --script tools/wasmdown.lisp run … --wasi` process; `root` becomes
`--dir ABS::/`, the run env becomes `--env K=V`, and proposal-tagged tests
raise instead of running unsupported (the driver excludes them by declared
rule). `tools/run-wasi-testsuite.py` (`make test-wasi-upstream`) is the lane:
it resolves the checkout (`--dir`, `WASI_TESTSuite_DIR`/`WASI_TESTSUITE_DIR`,
default `third_party/wasi-testsuite`), requires `git rev-parse HEAD` to equal
the new `specs/lock.toml [suite.wasi-testsuite]` pin
(`9d84631e427729802085c3d172d0a63cd360a1fa`, branch `prod/testsuite-base`,
2026-10-30), regenerates `tests/wasi-upstream-expectations.toml` from the tree,
runs upstream `run-tests`, and writes a deterministic
`tests/wasi-upstream-results.json` plus `tests/wasi-upstream-summary.md`.
Expected skips are compared against the runner's actual skips and the
denominator against the `*.wasm` count, so a new upstream test with a
proposal cannot be silently skipped; a missing/mismatched checkout exits 2
with the fetch recipe.

Measured after the 2026-11-02 mixed-release and host-contract fixes:
**127 guests, 95 pass / 0 fail / 32 declared skips** (28 / 67 when first
measured, 72 / 23 before the mixed-release composition). Skips: 18
`proposal-http`, 12 `proposal-sockets`, 2 P1 legacy `sock_shutdown-*` (network
communication is excluded by decision) — no failing test is skipped.

- **wasm32-wasip3 — clean: 25 pass / 0 fail / 30 declared skips.** The 23
  tests that were LINK-refused are admitted by the composed mixed-release
  graph (below); the host-contract defects they then exposed are fixed:
  - **CLI `argv[0]` is the basename.** The upstream `cli-env` guest compares
    `get-arguments()[0]` with `cli-env.wasm`; wasmtime supplies the module's
    file name, not the path. `%component-context` now sets argv[0] from
    `file-namestring`, and the P3 lane's `{fixture}` placeholder expands to
    the same file name (the pinned `p3-cli-guest` still asserts the supplied
    argv[0] byte for byte).
  - **Empty path is `no-entry`.** POSIX resolves an empty path to ENOENT for
    every `*at` call, and the upstream filesystem tests assert it. The old
    conservative "empty -> invalid" short-circuit is gone from
    `%path-components`; NUL stays `invalid`.
  - **Directory `get-flags`.** A directory reports `read` plus the rights it
    has (`mutate-directory`), never the file-only `write`; a directory opened
    with no descriptor flags reports `read`. A file's flags are the
    EFFECTIVE access mode: an explicit `write` without `read` is write-only,
    while `create`/`truncate` imply write without suppressing the read
    default.
  - **`advise` on a directory is `bad-descriptor`** (posix_fadvise applies to
    files; the test asserts the refusal).
  - **`set-size`** no longer invents `read-only` (the test accepts
    invalid/bad-descriptor/access and rejects read-only): the kernel decides,
    and a `filesize` beyond off_t is `file-too-large` instead of a host type
    error. `pread`/`pwrite` offsets at or above 2^63 are `invalid`.
  - **`stat` timestamps** are the struct's own atime/mtime/ctime slots; the
    record used to echo the mtime for all three, so the access timestamp
    could not be read back after `set-times-at`.
  - **`insecure-seed` is stable per provider instance.** The WIT says the
    function is called once and will become a value import; the reference
    runtime answers a second call with the first value, and the test asserts
    `a == a1`.
  - **Zero-length host reads complete.** A zero-length `read-via-stream`
    buffer against a host source used to park forever (the canonical
    End.copy "other end has no buffer" case); the host source now answers
    COMPLETED with progress 0, which is what the guest's empty-buffer read
    means.
  - **Newest exported `run` wins.** A wasip3 component exports both
    `wasi:cli/run@0.2.0` (a compatibility shim whose core entry is the test's
    `main`, i.e. `unreachable!()`) and the real `wasi:cli/run@0.3.2`.
    `component-run-export` used to take whichever the export hash visited
    first; it now sorts the versioned `wasi:cli/run` exports (WIT version
    order) and drives the newest, falling back to an unversioned instance or
    a direct `run` export.
- **wasm32-wasip1 — clean: 70 pass / 0 fail / 2 declared skips.** AssemblyScript
  12/12, C 12/12 (the two `sock_shutdown-*` tests are declared skips), Rust
  46/46. The fixes behind the jump from 26/44: preopen BASE rights are now the
  directory rights only while INHERITING carries the file authority (a
  directory base no longer advertises `fd_write`, so wasi-libc's
  `max & inheriting` no longer makes an `O_DIRECTORY` open `O_RDWR`/`EISDIR`);
  path lookup is handle-relative instead of always resolving from the preopen
  (with `..` confined to the handle); directory descriptors drop
  seek/tell/allocate/truncate rights; `fd_readdir` rewinds the shared
  directory offset, reports the real `.`/`..` inodes and pages by truncating
  the last dirent; filestats read nanosecond timestamps via `statx`;
  `poll_oneoff` uses the 48-byte subscription stride and mutates revents in
  the shared plist; `linkat`/`utimensat` no longer receive
  `AT_SYMLINK_NOFOLLOW`/`AT_SYMLINK_FOLLOW` where the kernel rejects them;
  paths containing NUL are `ILSEQ`, absolute symlink targets are
  `NOTCAPABLE`, and `fd_renumber` requires an existing destination.

### The mixed-release provider graph (2026-11-02)

The upstream precompiled wasip3 guests declare BOTH release lines in one
component: their `@0.3.2` imports (`wasi:cli/{environment,stdin,stdout,
stderr,exit,terminal-*}`, `wasi:clocks/*`, `wasi:filesystem/*`,
`wasi:random/*`) come from the 0.3.2 world, while the wit-bindgen p2
compatibility layer adds `@0.2.9` (occasionally `@0.2.0`) imports of
`wasi:io/{poll,error,streams}` and the `wasi:cli/*` façade. The runtime used
to refuse that shape (`component mixes WASI release lines …; no single
provider graph applies`) because no single world describes it.

`component-command-providers` now composes ONE graph
(`%compose-mixed-release-providers`):

* the declared imports that are NOT exact `0.2.x` names are the 0.3 line and
  are bound from `%wasi-provider-builders`, in declaration order, after the
  0.3 gates: `@unstable` feature refusal, `wasi:http` (canonical message) and
  `wasi:sockets` (canonical message) — no provider is built for a refused
  import;
* the exact-version `0.2.x` imports are bound from the P2 tables
  (`preview2-command-provider`) per DISTINCT patch version, exactly as the
  0.2-only path always did;
* both are merged in the component's declaration order into one `with-args`
  alist. A types-only 0.3 import needs no entry (as in world composition); an
  import neither line can serve is still a construction-time `WIT-ERROR`
  naming it. Nothing is invented to make a mix compose, and the 0.3-only path
  still composes the whole command world, the 0.2-only path still its
  version-exact tables (pinned by
  `wasi/component-pure-lines-are-unchanged-by-mixed-support`).

Pins: `wasi/component-mixed-lines-compose-one-graph` (both line names bound,
distinct provider instances, nothing undeclared), `...-keep-declaration-order`,
`...-sockets-declared-import-still-refused`, `...-http-declared-import-still-refused`
(0.2 and 0.3 spellings), `...-unstable-declared-import-still-refused`,
`...-unknown-line-import-is-construction-error`.

The lane is evidence, never a release gate (it is not part of `verify`), and
`declared_support` stays `partial`.

## HTTP and network restored, and the full upstream suite (2026-11-02)

The `feat/http` line supersedes the two exclusion sections above (“Network
communication excluded” and the M17 HTTP removal). HTTP and TCP/UDP/DNS are
now part of the declared WASI profile. This section is the current record;
the earlier exclusion text is kept only as history.

**Sockets.** `wasi:sockets/types@0.3.2` (`error-code`, address types, the
`tcp-socket`/`udp-socket` resources) and `wasi:sockets/ip-name-lookup@0.3.2`
bind for guest-declared 0.3-line imports. Authority is declared, never
ambient: the CLI `--net HOST:PORT` / `--listen ADDR:PORT` / `--dns NAME`
flags (repeatable, `*` wildcards) build a `wasi-network`; an embedder uses
`make-wasi-network`. With no declaration every bind, connect, UDP send and
resolve is refused through the interface's own `access-denied`. Readiness is
a bounded 2 s poll, not a socket reactor. The last legacy P1 gap,
`sock_shutdown` on a non-socket, was fixed by mapping the filetype check to
`ENOTSOCK` (Wasmtime's own adapter has the reference behaviour).

**HTTP.** The 0.3.2 `types` surface was restored (`fields`, `request`,
`request-options`, `response` with header/body/trailer semantics) together
with `client.send` over HTTP/1.1 on the built-in plain TCP transport.
Request bodies stream through `client.send` (not just pre-staged bodies) and
chunked responses carry trailers. The incoming direction is
`wasi:http/handler` + the `service` world: `serve-component` instantiates the
world's provider graph, binds a loopback listener, constructs the incoming
`request` resource (method, path, scheme, authority, headers, buffered body),
calls the guest handler through `lifted-func-call`, and serializes the
returned `response` (status, headers, buffered body). The CLI exposes it as
`wasmdown serve <component> --wasi [--addr HOST:PORT]`, which prints
`http://HOST:PORT` as its **first stderr line** (default `127.0.0.1:0`,
kernel-assigned port) and serves until SIGINT. TLS stayed out of the
runtime: `http` rides the plain transport, and `https` requires an
embedder-registered `wasi-http-transport` on the context; without one
`send` returns `configuration-error`, never a plaintext fallback.
`--http-transport` is refused by the CLI because transports are CL objects,
not a CLI capability. The `middleware` world's host-supplied `next`
composition has no runtime evidence and is **not** claimed; the `service`
world and exported handler are the exercised incoming surface.

**Mixed 0.3/0.2 providers.** The upstream wasip3 fixtures declare both
release lines. `serve-component-providers` and
`component-command-providers` compose the 0.3-line declared imports from the
0.3 builders and the exact-version `0.2.x` imports from the P2 tables under
one `with-args` alist; HTTP and 0.3 sockets are admitted on the 0.3 line
while `@unstable` gating and P2 version-exact selection stay in force.

**Network lane.** `tools/setup-network-tests.sh` clones the pinned
`wasi-testsuite` and fetches a reference Wasmtime into `.cache/`, checks the
pinned HEAD, installs the upstream runner's Python dependencies and runs a
loopback/HTTP-framing smoke. `tools/run-wasi-network-tests.py`
(`make test-wasi-network`) selects the inverse of the old exclusion set —
every guest whose JSON declares the `http` or `sockets` proposal is
admitted, every other test is a generated non-network skip — and runs it
through `adapters/wasmdown.py`: **30/30**. `make
test-wasi-network-reference` runs the same subset through upstream Wasmtime
as a host-harness smoke: **28/30**, the two failures being known
Wasmtime-50 issues, so it is report-only.

**Full upstream suite.** With HTTP and sockets admitted and the
`ENOTSOCK` fix in place, `tools/run-wasi-testsuite.py`
(`make test-wasi-upstream`) no longer generates exclusion expectations: at
the pinned commit `9d84631e427729802085c3d172d0a63cd360a1fa` the lane
measures **127 pass / 0 fail / 0 skip** (AssemblyScript 12, C 14, Rust
wasip1 46, Rust wasip3 55). This is the F1 evidence substitute for a locally
built @0.3.2 guest; no toolchain here emits that component ABI, so
`declared_support` remains `partial` under §12.11's blocked-by-environment
rule. The counts previously recorded for this lane (95/0/32 and 70/0/2)
were real measurements of the exclusion policy and are superseded, not
rewritten.

Evidence at the change: units 726/0, component 1,702/0, P3 30/30 ×3 in both
backends, upstream 127/0/0, network subset 30/30, `make verify` green.

**WASI 0.2 outgoing HTTP (2026-11-02).** The Preview2 line now serves the
outgoing HTTP import surface at every vendored patch release (`0.2.0` …
`0.2.12`): `wasi:http/types` exposes all resource types and methods
(`fields`, `incoming-request`, `outgoing-request`, `request-options`,
`response-outparam`, `incoming-response`, `incoming-body`,
`future-trailers`, `outgoing-response`, `outgoing-body`,
`future-incoming-response`, `http-error-code`) and
`wasi:http/outgoing-handler.handle` returns a pollable
`future-incoming-response`. The P2 providers reuse the 0.3 HTTP/1.1
transport, network authority, request validation and body decoder: a P2
`outgoing-request` rep is the 0.3 request rep, its body writes through a
host sink into the streamed request contents, and an `incoming-response`
rep is the 0.3 response rep whose bytes are decoded on a bounded,
backpressured body pump. The old `wasi:http` refusal is gone; the P2 builder
table has `types` + `outgoing-handler`, and `preview2-proxy-provider`
composes the resolved `wasi:http/proxy` world's cli/io/clocks/random +
HTTP imports. `wasi:http/incoming-handler` (serve side) remains the peer
slice. Evidence: P2 unit file 38/0, full unit suite 734/0, the generated
backend inventory now reports `P2 0.2.12 pinned=32 stable=31 bound=29
unbound_stable=2`, and no 0.3 HTTP/sockets test regressed.


**WASI 0.2 incoming HTTP and proxy serving (2026-11-02).** The Preview2
line now serves the incoming side too: the proxy world resolves,
`preview2-serve-providers` composes its io/clocks/random/cli + HTTP imports
at each declared exact 0.2.x patch version, `preview2-serve-p` and
`p2-incoming-handler-export` detect the newest exported
`wasi:http/incoming-handler@0.2.x` `handle`, and `wasmdown serve`
dispatches 0.2 proxy components vs 0.3 service components. The server parses
HTTP/1.1 (Content-Length and chunked bodies with trailers), builds an
`incoming-request` resource plus a `response-outparam`, invokes the sync
handler, captures `response-outparam.set`, consumes the outgoing response
body/trailers through the P2 stream resources, and serializes status,
headers, body and trailers; malformed requests are 400, unset/trapping
handlers 500. Inventory records incoming-handler as export-only at every
release (`P2 bound=29`, two export-only world entries). Evidence: unit
739/0 (P2 provider completeness, response-outparam capture, chunked
request/response trailers, loopback request→handler→response), P2 lane
11/11, P3 10/10, upstream 127/0/0, `make verify` green. No vendored 0.2
proxy guest binary exists, so the loopback test drives a host handler
closure; real-guest body streaming under a compiled 0.2 producer remains
unmeasured.

**Pinned WASI 0.3 toolchain guest (2026-11-02).** The environment now has a
real generated-guest toolchain: `tools/setup-wasi-toolchain.sh` installs Rust
`nightly-2026-11-01` (rustc 1.101.0-nightly) with the `wasm32-wasip3` target
and wasm-tools 1.263.0 under the git-ignored `.cache/`.
`tests/toolchain/wasi-guest` is a small command guest (argv/env/stdout);
`make build-toolchain-guest` builds it and `tools/normalize-wasi-version.py`
rewrites the target std's same-length `@0.3.0` interface labels to the
vendored `@0.3.2` release before `wasm-tools validate` and hash pinning
(`tests/fixtures/wasi/toolchain-guest.wasm`, `TOOLCHAIN-CHECKSUMS`).
`make test-toolchain-guest` runs the tracked artifact in both the interpreter
and compiled backends with exact stdout. This closes the §12.11 upstream
toolchain-guest item, so `wasi_0_3_profile = conformant`; global
`declared_support` stays `partial` for the broader engine/legacy scope. The
full upstream wasi-testsuite remains 127/0/0. Evidence: `make verify` green,
including the toolchain guest.


**curl as a WASI component (2026-11-02).** `tools/setup-curl-wasm.sh` builds
real curl 8.22.0 with wasi-sdk 34 (`wasm32-wasip3`, HTTP-only, no TLS/zlib)
under `.cache/`; it patches curl's optional SIGALRM setjmp include out
(wasi has no signals) and links `-lwasi-emulated-signal`.
`make test-curl-wasm` starts a loopback HTTP server and runs the component
through `wasmdown` with `--net 127.0.0.1:PORT -- ... -sS -v URL`, asserting
the exact body; the test is green (21 bytes). Residual: quiet mode
(`-sS` without `-v`/`--trace`) can remain parked after the last body byte in
the current async scheduler, so the reproducible test uses verbose mode.
`make verify` remains green; this is an ecosystem smoke, not a new
conformance claim.


**WASI 0.2 real guests for sockets and HTTP (2026-11-02).** The P2 providers
were host-tested but had no vendored component. Two real guests now exist:
`tests/toolchain/p2-sock-guest.c` (wasi-sdk 34, `wasm32-wasip2`, TCP connect
+ send + recv) and `tests/toolchain/p2-http-guest` (cargo-component, target
`wasi:http/proxy@0.2.12`, incoming-handler returning a body). Building them
exposed two P2 socket defects: the create-socket result carried the 0.3
resource ct instead of the exact 0.2 `tcp`/`udp` ct (handles then failed
`host-resource-rep`), and P2 `error-code` is an ENUM while the native
helpers carry the 0.3 variant shape (the result serializer tried to
`string` a case list). Both are fixed; `make test-p2-guests` runs the C
TCP client against a loopback echo and the proxy guest through
`wasmdown serve` + HTTP GET (21-byte body), both PASS. `make verify`
includes the lane. This upgrades the P2 facade evidence from host-only to
real guest execution; two export-only stable entries remain accounted for.


**Threads/atomics proposal as an opt-in SBCL-thread extension (2026-11-02).**
`wasmdown:*threads*` (also CLI `--threads`, `WASMDOWN_WAST_THREADS=1`)
enables shared memory and 67 atomic instructions while leaving the default
single-threaded behavior byte-for-byte unchanged. Binary/text/validation
accept the shared memory flag (maximum required) and the 0xFE atomic family
with natural-alignment checks; interpreter and codegen lower the same
runtime primitives. Atomics are serialized by a per-memory SBCL mutex;
`memory.atomic.wait32/64` block on a per-address waitqueue and
`memory.atomic.notify` wakes up to N waiters (returns 0 woken / 1 not-equal /
2 timed-out). The WAST runner spawns real `sb-thread`s for `(thread ...)`,
shares the referenced module/memory for `(shared (module $M))`, joins on
`(wait ...)`, and propagates thread failures. Vendored WebAssembly/threads
`cc535ada…` 13-file suite: **551/551 commands in both interpret and compiled**,
29 threads, no green scheduler. Atomic lane also 372/372 in both backends.

**WASI Preview1 wasi-threads spawn (2026-11-02).** The core threads
extension now has the matching WASI-level spawn API, still behind
`wasmdown:*threads*` / CLI `--threads`. With the gate on, the Preview1
provider additionally registers the legacy proposal's single import
(`wasi` / `thread-spawn`; also exposed as
`wasi_snapshot_preview1` / `wasi_thread_spawn`) with signature
`(start_arg: u32) -> s32`. `wasi_thread_spawn` instantiates the SAME
module again with the same caller import entries (so an imported shared
memory is literally the same object) and fresh Preview1 host functions
closing over the same state/context/fdtables. A module-defined ABI
memory is replaced in the child instance's memory vector and export
entries by the parent's `memory-instance` before its start section, so
atomics and plain loads observe one linear memory; the child's own
active data segments were applied to the throw-away memory and are
deliberately not replayed over live thread state. The exported
`wasi_thread_start(tid, start_arg)` is type-checked, a unique TID in
`[1, 2^29)` is allocated under a per-state mutex, and a real SBCL
thread calls the child export. Spawn failure (no/mistyped export,
non-shared memory, closed/terminating state, TID exhaustion,
instantiation error) returns `-1`; wasi-libc maps any negative to
EAGAIN. Termination follows the proposal: return from
`wasi_thread_start` ends only that thread; a trap or `proc_exit` in any
thread aborts every thread, including a main thread parked in
`memory.atomic.wait` (a per-memory abort condition is signalled by the
wait, and waiters are woken); returning from `_start` is proc_exit(0).
State close terminates and joins children before closing fds, with a
force-terminate fallback for a guest that never waits. Test evidence:
8 new unit tests (`tests/wasi-preview1.lisp`, 769/0 total) plus
`make test-wasi-threads-p1`, which compiles six hand-authored WAT
fixtures with wasm-tools and runs the real CLI over shared-memory
visibility, TID/arg passing, the negative spawn error, child
proc_exit(99), child trap (status 1) and main-return teardown, each
under a wall-clock timeout. The 46-import Preview1 census and all
default (threads-off) behavior are unchanged.
