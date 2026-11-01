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
- **Honest reclassification.** Unsupported rose from 277 to 725 and then settled: actions against genuinely out-of-scope modules classify cleanly instead of crashing as harness errors. The 73 that remain are terminal — the annotations proposal (`@…` forms), unadopted and permanently out of scope per [`design/PLAN.md` §3.3](design/PLAN.md). Every family that used to be unsupported (struct/array declarations, rec groups, typed function references, exception handling, named modules, tail calls) now passes.
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

The richer stdout-importing async command already in `tests/wasi.lisp` writes
"hi" through a `stream<u8>` and `task.return`s; it is green through
`run-component` without a reactor. Its body is not resume-safe (it writes
unconditionally), so a reactor-driven resume re-executes it and stdout repeats;
that is a limitation of this four-instruction fixture, not of the scheduler,
and it is recorded in `status.md`. The CLI lane uses the sync command until a
resume-safe async guest is pinned.

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
  leaked threads. The lane runs four guests, 3 repeats each, in both
  backends. The component binary grammar details that mattered: type indices
  inside an instance type are local to that type's declarations, the lower's
  core func must be defined before the bag that exports it, and imported
  instances occupy the instance index space before defined ones.

## Roadmap-era notes

- Numbering convention: engine-slice loops 18–24 and Component Model milestones M7–M17 ran in parallel; neither series renumbers the other.
- Remaining work named at the M17 close, none of it inside the lane: provider step 5/6 (DNS and blocking-stdio bounded executors, and socket accept/recv/send registering a reactor watch instead of reporting would-block to a caller that then waits); step 1's last item (the two canon-lower wrappers' positional stream/future/borrow arms, where deletion needs the verification-set fixtures in the probe set first — `%abi-load`'s memory-path transfer arms stay out as a measured negative); borrow-side lifetime pins (a live borrow held across `task.return` must trap; borrow-in-awaitable-set), reusing the guest-drop pin's shape; and one parked delta with a measured verdict, `wip/type-view` (at parity in a clean window, not landed because a green lane's bar is "no measured cost"). `wip/reactor-wiring` was closed as superseded: its driver pump and context reactor landed as the store driver, its socket plumbing as the executor slice with a strictly stronger keying scheme, so it was never rebased or re-measured.
- Residual security/ABI honesty items recorded with M16: no socket or timer reactor, so a guest cannot be woken later and `clocks.wait-until`/`wait-for` stay declared-unsupported; the nested `option<stream<u8>>` through a host lower resolves the async end in the provider's table instead of the caller's; descriptor-destructor and lend-list accumulation for long-lived hosts; `%abi-lower`'s top-level error-result discriminant for compound outer shapes; pre-existing text/validator limits around imported-instance functypes with outer compound refs (the runtime lifts with the real WIT functype, so fixtures work around the validator, not the ABI); `@unstable` gating parsed but not enforced (the timezone interface is simply not registered); `wasi:sockets` has no `shutdown` in this WIT revision; and world `include`, `implements`/`external-id` and nominal resource typing are not read by the WIT frontend.
- Items open since the M7-era measurement: the compiler-backend parity matrix (§12.3's `:optimized`/`:aot` plus the differential oracle), the property/fuzz lanes (§12.6), and the remaining CI lanes (M11). Threads/atomics and the FASL cache were never started.
