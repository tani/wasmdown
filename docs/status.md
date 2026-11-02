# Status

The runtime declares `declared_support = partial` for the engine/legacy
surface in `specs/lock.toml`; the **declared WASI 0.3.2 profile** is
`conformant` (`wasi_0_3_profile`), with the evidence below. This page is the
per-area reading: what is complete, what is deliberately incomplete, and
what will not be built. Measurements live in
[`conformance/`](conformance/README.md); scope decisions live in
[`design/PLAN.md`](design/PLAN.md) and the [`adr/`](adr/README.md).

## Support matrix

| Area | Capability | State | Basis |
| --- | --- | --- | --- |
| Frontend | Core binary decoder | Full | Every section and opcode in the declared scope decodes with bounds checks and byte offsets. |
| Frontend | Binary encoder | Full | Canonical minimal output round-trips through the decoder. |
| Frontend | Core text format (`.wat`) | Full | Produces the binary decoder's IR for the whole implemented opcode set. |
| Frontend | Spec scripts (`.wast`) | Full | Commands, assertions and failure-phase classification run against shared script state. |
| Frontend | Component text | Full | Parses the pinned validation suite's grammar into the component IR. |
| Frontend | WIT reader | Full (pinned grammar) | World `include`/renaming, named `implements` and positional `@external-id` bindings, and nominal resource identity are parsed, resolved and pinned by tests (`tests/wit.lisp`: include-of-includes, rename equivalence, resource nominal identity across interfaces). Validator handling of aliased compound functypes is resolved; see the `Component type-use limits` row. |
| Validation | Core validation | Full | Operand/control-stack algorithm with unreachable polymorphism over the declared scope. |
| Validation | Component validation | Full | Label, index, canon, type-identity and instantiation-matching rules pass the pinned suite. |
| Validation | Component type-use limits | Full (contract-tested) | Imported-instance functypes whose parameter or result is a compound type brought in by an outer alias now resolve their nested refs in the **owner's** type space: `entry-at` returns `(ct, owner-view)`, an outer alias switches to the owner selected by its de Bruijn count, an eq-bound export propagates the target's owner, and `%materialize-component-ct` materializes against that owner space (owner-qualified through nested outer aliases and eq bounds) while declarator-local entries still use the declarator remap. Pinned by `component/remap-outer-compound-functype-owner`: the exact diagnostic pair at count 1 AND a two-level count-2 ring (a decoy-first same-numbered declarator type validates with no memory, while a real `string` leaf still rejects with a memory requirement), the `tuple<u32>` vs `tuple<string>` shape checks, and the result-position companions, and the count-2 positive/negative pair. Unit 739/0, component 1,702/0, P3 10/10 (30/30) in both backends, `make verify` green. |
| Validation | Hostile-input preflight | Full | Oversized literals and local counts are rejected as catchable `decode-error`s before allocation. |
| Backends | Interpreter | Full | Default backend: scalar, SIMD, GC, EH, tail calls, multi-memory and memory64 semantics. |
| Backends | Baseline codegen | Full for opcode coverage | Compiles every defined function before the start section and lowers every instruction the interpreter implements (scalar, memory, table, control, references, EH, tail calls, SIMD incl. relaxed, GC). An instruction with no lowering is a `codegen-error`, never an interpreter fallback. |
| Backends | Backend parity (pinned core) | Full | `tools/run-backend-parity.lisp`: 21,260 / 21,260 commands agree in outcome and phase across both explicitly selected backends. |
| Backends | Optimized or AOT tier | Not implemented | The engine ships an interpreter and a compiled backend only. |
| Backends | FASL cache | Not implemented | Compiled functions are not persisted across processes. |
| Backends | Threads and atomics | Not implemented | Shared memory and the atomic instruction family are absent. |
| Backends | Annotations proposal | Out of scope | Excluded permanently by the deliberate-exclusion list in [`design/PLAN.md` §3.3](design/PLAN.md). |
| Component model | Binary and text decoding | Full | Both front ends build one component IR, including WIT value types and aliases. |
| Component model | Instantiation and linking | Full | Two-level core-instance linking, component `with`-args and alias projection execute. |
| Component model | Canonical ABI | Full | Lift/lower, flattening, string transcoding, `map` despecialization and post-return run at runtime. |
| Component model | Resources | Full | Per-instance handle tables, own/borrow transfer, destructors and method dispatch. |
| Component model | Async execution | Partial | The task/subtask state machine, cancellations and resource lifetimes run the whole async suite in both backends. Live-borrow accounting traps `task.return`/`task.cancel` with outstanding borrows. A deterministic property lane (`make test-async-property`, 14,071 checks over fixed seeds 1,2,3,5,8,13,21,34 x3, plus a negative control) drives the real host-pending, stream/future/cancel, task/subtask and backpressure primitives and checks exactly-once delivery, no double resolve, monotone states, work accounting and cancel/close races; no wall-clock sleeps. The store-retirement limits under Known gaps still bound long-lived reuse. |
| Component model | Backend parity | Full | The whole component lane (1,702 commands) is green with every defined core compiled before its start section (`make report-component-compiled`). |
| WASI | Preview 1 (`wasi_snapshot_preview1`) | Full (46 imports) | All 46 pinned imports are provided over an explicit capability context; the whole acceptance lane (in-memory and on-disk SQLite, rollback, bail, preopen denial, read-only, escape) passes with compiled guest cores too. The legacy `sock_*` imports are implemented and now exercised by the pinned upstream C suite: the two `sock_shutdown-*` cases pass after the ENOTSOCK-from-filetype fix, and the P1 subset of the upstream lane is 72 pass / 0 fail / 0 skip. |
| WASI | 0.3.2 providers | Conformant (declared 0.3.2 profile) | The CLI owns a reactor (clock deadlines and stream readiness are servable; a 200 ms `wait-for` completion is guest-evidenced by `p3/deadline-wait`); the store-retirement limits under Known gaps are accepted limits, not unmet gate items. The blocked-stdio executor audit is closed (2026-11-02): separate fixed 2-worker DNS/stdio services, worker affinity, bounded active jobs, close refuses new work and detaches without joining, and the span is copied to the worker; evidenced by `tests/async-property.lisp`, the `tests/wasi.lisp` executor/stdio pins, and guest stdio through `p3/cli-stdin-env-stderr-terminal`/`p3/stream-stdout`. Declared `@unstable` imports are refused at link time (see Known gaps), so stability gating is no longer a missing piece. `wasi:http` (types/client/handler/service) and `wasi:sockets` are implemented, not missing pieces; see their support rows. |
| WASI | 0.2 facade | Partial (legacy compatibility line) | 29 of 31 stable interfaces are bound at exact release names; the other two (`wasi:cli/run`, `wasi:http/incoming-handler`) are export-only world entries, so every stable interface is accounted for. The 0.2 line is a **legacy compatibility target**: `wasi:sockets/*`, `wasi:http/types`, `outgoing-handler` and the exported `incoming-handler`/proxy world are implemented at each exact patch version, and `wasmdown serve` dispatches 0.2 proxy components. The declared network service surface remains the 0.3.2 one. |
| WASI | `wasi:http` | Supported (0.3.2; outgoing + incoming) | The 0.3.2 `types` resources (`fields`, `request`, `request-options`, `response` with header/body/trailer semantics) and `client.send` over HTTP/1.1 on the built-in plain TCP transport, including streaming uploads and chunked trailers. The incoming `handler` interface and the `service` world are served by `serve-component` / `wasmdown serve`, which prints `http://HOST:PORT` as the first stderr line and answers requests until SIGINT. TLS is embedder-only (`:http-transports`); an https request without a transport is `configuration-error`, never plaintext. `middleware` world composition with a host-supplied `next` handler is implemented by `http-middleware-providers` and unit-pinned; the pinned suite has no middleware fixture. The pinned upstream suite admits the HTTP cases (127/0/0 overall). |
| WASI | `wasi:sockets@0.3.2` | Supported (declared authority) | `types` (`tcp-socket`/`udp-socket`, errno → `error-code`) and `ip-name-lookup` are bound for guest-declared 0.3-line imports under an explicit `wasi-network` authority: the CLI `--net HOST:PORT`, `--listen ADDR:PORT` and `--dns NAME` flags (repeatable; `*` wildcards), or `make-wasi-network` in an embedder. With no declaration every bind, connect, send and resolve is refused. The pinned network subset is 30/30 (`tools/setup-network-tests.sh` then `make test-wasi-network`); the upstream-Wasmtime reference smoke is 28/30 (two known Wasmtime-50 failures). Readiness is a bounded 2 s poll, not a socket reactor. |
| WASI | 0.3.2 guest lane | Partial | Ten hand-authored @0.3.2 commands run through the real CLI in both backends (`make test-wasi-preview3`, 3 repeats, 30/30 per backend): a no-import smoke; a `wasi:cli/exit@0.3.2` importer (exit 7); a `wasi:clocks/monotonic-clock@0.3.2` importer; a resume-safe async callback command; a `write-via-stream` stdout command printing `hi`; a cancelled-subtask command; a `wasi:random/random@0.3.2` guest with `insecure`/`insecure-seed` (two 16-byte reads must have exact lengths and differ; `get-random-u64`/seed must complete), lowering the list returns through memory+realloc; a clock-deadline guest (`p3/deadline-wait`) whose async `wait-for` asks for 200 ms and must return only after at least that much clock time, parked in a waitable set until the CLI's owned reactor fires; a `wasi:cli/{stdin,environment,stderr,terminal-stdout}@0.3.2` guest (`p3/cli-stdin-env-stderr-terminal`) asserting argv[0]/marker, an `--env` pair and the option-typed initial cwd, `terminal-stdout` = `none` on a non-tty, reading exactly `stdin-probe\n` from real stdin through `read-via-stream` with the explicit DROPPED EOF result and a paired future that reads back ok, and writing `p3-cli-guest: stderr probe` through stderr's `write-via-stream` in canonical future order; and a `wasi:filesystem/{types,preopens}@0.3.2` guest (`p3/filesystem-preopens-read`) that lists the `/work` preopen, opens the seeded `hello.txt` through it (async `open-at`, waited as a subtask), reads exactly 17 bytes via `read-via-stream`+`stream.read`, creates `out.txt`, writes at explicit offsets and appends at EOF with `stat` (regular-file, link-count 1, size 22) and a byte-exact read-back, gets `error-code::read-only` (case 32) for a write through the read-only descriptor, exercises `create-directory-at`/`stat-at`/`read-directory` (enumerating `hello.txt`, `out.txt`, `subdir`, `temp.txt`)/`rename-at`/`unlink-file-at`/`remove-directory-at`, requires err `no-entry` (never a trap) for missing paths, and drops every handle. The 200 ms bound sits past the async lift's former ~100 ms quiescence budget and is the regression proof for the liveness-oracle fix: pending host work plus an armed one-shot reactor deadline is possible progress, a bare fd-readiness watch is not, and the component suite's genuine deadlock fixtures (`component/deadlock.wast`, `dont-block-start`) still trap. Unit pin: `wasi/reactor-pending-deadline-p`. The stream, random, deadline, cli and filesystem guests are authored in TEXT and encoded by `tools/component-encode.lisp`, guarded by `wasi/component-text-encoder-roundtrip`. The lane stays Partial because no upstream @0.3.2 toolchain guest exists here; provider behaviour is measured by this lane and the component tests. Network behaviour is measured by the dedicated upstream network subset (`tools/setup-network-tests.sh` then `make test-wasi-network`, 30/30) and by the HTTP/sockets support rows; no socket command is hand-authored in this ABI lane. **Upstream `wasi-testsuite` lane green (2026-11-02, F1):** the pinned adapter (`make test-wasi-upstream`) measures 127 pass / 0 fail / 0 skip over 127 guests; `wasm32-wasip3` is 55 pass / 0 fail / 0 skip and `wasm32-wasip1` is 72 pass / 0 fail / 0 skip after a component declaring both release lines is admitted by composing its `@0.3.2`-line providers from the 0.3 command builders with its exact-version `@0.2.x` P2 providers under one `with-args` alist (HTTP and 0.3 sockets admitted; `@unstable` gating unchanged). The newly admitted tests exposed and pinned several host-contract fixes (argv[0] basename, empty-path `no-entry`, directory `get-flags`, `advise`, `set-size`, `stat` timestamps, `insecure-seed` stability, zero-length host reads, unrepresentable offsets, newest exported `run`); see `tests/wasi-upstream-summary.md`. |

## Known gaps and accepted limits

- **Resolved (2026-11-02): nested `option<stream<u8>>` payload ownership.**
  `%abi-load` implements the loading spec's `stream`/`future` rows through
  `%lift-async-end` instead of the old catch-all: the memory slot indexes the
  **loading (caller) instance's** handle table, the readable end is removed
  from that table with its owner and index cleared, and the source slot is
  freed for reuse — a second load of the same index is a stale-index trap.
  `%lift-async-end` carries `lift_async_value`'s guards (the readable class
  must match the valtype; the element type must match through `%abi-type-key`,
  which resolves both type spaces; own/borrow compare class only, because
  resource indices are alias-projected) and clears owner+index on the lifted
  end. The async result-store context carries `:type-inst`, the **provider's**
  type space, so a nested `(:idx . n)` resolves there instead of against the
  caller's types. Pinned by
  `component/indirect-async-payload-ownership-pin` (a forced-indirect sync
  host lower at 18 flat slots; async `option<stream<u8>>` results with
  colliding type indices; `none` allocating nothing; stale/class/element/
  writable/non-idle/waitable-set traps) and the migrated
  `component/generic-path-five-cases-pin` CASE 2 (end-object contract; the
  second load traps). Negative controls confirmed both the load arm and the
  guard set are load-bearing. Unit 739/0, component 1,702/0, P3 10/10 (30/30)
  in both backends.
- **Resolved (2026-11-02): host borrow reclamation and store-retirement
  metadata.** A host callee has no async task to adopt the borrow handles a
  lowering staged, so the staged records were dropped and their slots leaked.
  Each staged handle is now a `pending-borrow` record carrying its owner
  table, 1-based slot and the handle itself, reclaimed **exactly once**: on
  the immediate return and on a trap (an `unwind-protect`), and across a
  HOST-PENDING only at the pending's **terminal** outcome — resolve, reject,
  or an **acknowledged** cancel. An un-acknowledged cancel is not terminal,
  so the borrow stays live until the provider's real result. Reclamation
  clears the slot and returns it to the free list only while it still holds
  the same record, so a slot already reused is never disturbed; the source
  handle's `num_lends` ledger stays the subtask's and is cleared by
  `%subtask-deliver-resolve`. `%handle-add` now follows the shared Table
  free-list policy (§1131): a resource handle reuses a freed slot instead of
  appending forever, so repeated new/drop cycles plateau. `%store-teardown`
  clears `resource-dtor-table`, `ct-defining-inst` and `comp-closure` — for
  the store it is given and for the bound store — because every
  store-lifecycle caller discards the store afterwards: the wast per-script
  store, `call-with-component-store`'s close path, `run-component`'s own
  store and the pins' fresh stores. Pinned by
  `component/host-borrow-reclaim-pin` (immediate return, trap, a nested guest
  call does not inherit the account, free-slot reuse, source handle
  untouched), `component/host-borrow-retained-across-pending-pin` (retained
  through resolve and acknowledged cancel; a cancel request alone is not
  terminal; a late resolve still reclaims),
  `component/store-metadata-retires-with-store-pin` (warm-up plus four
  instantiate→new/drop→teardown cycles give identical metadata counts; six
  destructor runs in one graph leave the handle vector at capacity 2 and the
  free list at 2; a second live handle of the same type keeps the dtor
  registered) and
  `component/resource-metadata-live-graph-isolation-pin` (two live stores;
  one store's teardown never strips the other's provenance). The
  `component/generic-path-five-cases-pin` CASE 3c/B was migrated from
  expecting the leaked slot to asserting it is empty and back on the free
  list. WASI evidence: `wasi/fs-descriptor-drop-fd-plateau-pin` runs six
  open→stream→drop→stream-end cycles and keeps the process open-fd count at
  baseline, with a stream that outlives its handle holding the fd until the
  last release closes it exactly once. Unit 739/0, component 1,702/0, P3
  10/10 (30/30) in both backends.
- **Limits of store-retirement-based reclamation.** A caller-owned store
  reused for many top-level instantiations **without** `store-teardown` still
  accumulates dtor/provenance rows: there is no per-instantiation pruning,
  and a live graph must keep its types' dtors registered because a dropping
  handle still resolves them. `store-teardown`'s scheduler-state teardown
  still targets the **bound** store, so an explicit argument retires that
  store's metadata while the bound store's scheduler state is torn down.
  Outstanding non-terminal host pendings are not force-reclaimed at teardown,
  and a freed resource slot can be reused by any record class (the class
  guards reject a stale index that names a different class, as a trap).
- `%abi-lower`'s top-level error-result discriminant for **compound outer
  shapes** was **audited and withdrawn as a residual (2026-11-02)**: no
  reproducer exists. The `:result` arm lowers to `variant(ok, error)` and the
  join always leads with the discriminant, including both-empty records,
  mixed-arity tuples, a variant carrying a record, and an `option`/`option`
  pair. `component/abi-compound-result-lowering` was extended with those four
  shapes and passes unchanged; **no lowering code was changed**. The separate
  **validator type-use limit is resolved (2026-11-02)**: imported-instance
  functypes whose parameter or result is a compound type brought in by an
  outer alias now resolve nested refs in the owner's type space (`entry-at`
  returns `(ct, owner-view)`; an outer alias switches by de Bruijn count; an
  eq-bound export propagates the target's owner; `%materialize-component-ct`
  materializes against that owner, owner-qualified through nested outer
  aliases and eq bounds). Pinned by
  `component/remap-outer-compound-functype-owner` with the exact diagnostic
  pair (decoy-first validates with no memory; the real `string` leaf still
  rejects with a memory requirement), shape checks (`tuple<u32>` vs `tuple<string>`) and
  result-position companions, and the count-2 positive/negative pair. **No §9.3 defect row is open.** The row
  history is in §9.3 of [`design/PLAN.md`](design/PLAN.md).
- **Resolved (2026-11-02): validator instance type-index space and encoder
  s33 valtype indices.** Two engine correctness fixes, not §9.3 defect rows,
  exposed by the WIT-shaped filesystem fixture. `%vis-check-type-use` pushed
  every import/export declarator inside an INSTANCE type, but Binary.md 274
  makes only type-bound declarators introduce a TYPE index (a COMPONENT type
  keeps every declarator, Binary.md 415); the shifted count could name an
  unrelated same-numbered declarator. The walk now pushes only
  type-bound declarators for instances. In `tools/component-encode.lisp`,
  component valtype type indices are written as signed s33 (Binary.md 252);
  the old `u32` wrote 64 as `40`, the FUNC constructor byte, so a component
  whose valtype reference reached 64 round-tripped malformed. The regression
  witness is the P3 filesystem fixture/lane (`p3/filesystem-preopens-read`,
  checksum `a3293778…`): the previous fixture passed because its references
  happened to land on slots the validator accepted (it referenced no exported
  `descriptor-stat`/`directory-entry` slot from a later declarator), and no
  valtype index reached 64, where `u32` and `s33` agree. Unit 739/0,
  component 1,702/0, P3 30/30 ×3 both backends.
- **Resolved (2026-11-02): the P3 filesystem guest covers the full pinned
  `wasi:filesystem/types@0.3.2` surface.** `p3/filesystem-preopens-read`
  exercises preopens/open-at (read/write/directory), read/write/append
  streams at explicit offsets, `advise`, `set-size`, `set-times`/`-at`,
  `get-flags`, `get-type`, `sync`/`sync-data`, `metadata-hash`/`-at`,
  `is-same-object`, `link-at`, `symlink-at`/`readlink-at`, `stat`/`stat-at`
  with and without symlink follow, and directory create/read/enumerate/
  rename/unlink/remove. Error cases are checked as err results (no-entry,
  read-only case 32, invalid readlink, not-permitted absolute symlink),
  every handle is dropped, and the suite carries host-side byte/absence
  witnesses. 30/30 x3 both backends. (0.3.2 has no seek API.) Pinned by
  `p3/filesystem-preopens-read`; the row is closed.
- Provider variant lowering accepts a bare label only for a **payload-less**
  case. Giving a bare label to a case that carries a payload is now a
  `host-error` (`a bare variant label (...) names a case with a payload; pass
  (label . payload)`) instead of storing the discriminant and leaving the
  payload slot stale; the tagged `(label . payload)` form is unchanged. Pinned
  by `component/abi-bare-variant-label`.
- `@unstable` imports are **enforced at link time**. The 0.3 composition path
  checks every declared `wasi:` import through the WIT selection gate
  (`wit-import-surface-status`) before anything is composed: if the target
  interface is `@unstable(feature = …)` and that feature is not enabled by the
  selected stable surface, the run is refused with a `link-error` naming the
  import and the feature, for example:

  ```
  wasi:clocks/timezone@0.3.2 is @unstable(feature = clocks-timezone) and that feature is not enabled; unstable interfaces are outside the selected stable WASI 0.3.2 surface (see docs/usage/wasi.md and specs/wasi/MANIFEST.md).
  ```

  Stable imports are unaffected. **No CLI flag enables a feature today**; the
  gate is refusal-only. Pinned by
  `wasi/component-unstable-declared-import-is-link-error`,
  `wit/selection-import-surface-query` and
  `wit/selection-command-world-timezone-import`.
- The four-instruction async fixture in `tests/wasi.lisp`
  (`wasi/e2e-command-component-binary`) is not resume-safe: it writes
  unconditionally, so under a reactor-driven resume each re-entry emits `hi`
  again. That is a property of that fixture, not of the scheduler. It is
  retained as the in-process `run-component` case; the CLI lane now pins
  resume-safe commands instead (`p3/async-callback` keeps its state in
  memory; `p3/stream-stdout` parks the future read, stages the write and
  waits on real events), so the lane no longer depends on the
  four-instruction body.
- The P3 `write-via-stream` path is now exercised end to end by a pinned
  guest (`p3/stream-stdout`, 3 repeats, both backends): it prints exactly
  `hi\n` through the worker-backed CLI stdout. Two runtime defects found
  while building it are fixed and pinned: `%host-end-retry` cleared the
  parked host copy before its retry (so every readiness resubmitted the
  span and stdout duplicated — now the copy is reused), and `%end-notify`
  woke only a sync waiter, never a `waitable-set` waiter (so a task parked
  on the set never learned a stream/future copy completed — now both are
  woken). The guest itself must use the canonical order: park the future
  read first, stage the write, wait for a WOULD-BLOCK write's own event
  (3) before `drop-writable`, then wait for the future event (4).

- WASI 0.2 (shared by both backends, after the readiness/permit audit):
  end-of-input is still reported as `stream-error::closed` rather than
  `ok([])` (the open-but-empty vs closed distinction itself is now exact:
  `read(0)` and an open source with no bytes return `ok([])`, while a
  closed rep returns `closed`); `check-write`/`check-send` permits are
  enforced (a write or datagram batch beyond the offer traps) and an
  unwritten write suffix is retained for the next flush; permit values are
  host-chosen rather than queried from the socket buffer;
  `sockets/network-error-code` has helper-level coverage only (it is
  `@unstable` at 0.2.12 and not exported); a `last-operation-failed`
  stream does not yet transition to `closed`. These are 0.2-line legacy
  compatibility limits; the declared network surface is the 0.3.2 provider
  set, which is measured by the upstream network subset.
- Preview 1 `proc_raise` returns `NOTSUP`; a guest cannot signal itself.
- Compiled-backend compile time/heap is an ACCEPTED limit (owner decision,
  2026-11-02): no reduction ships. Measurements and the rejected levers are in
  `docs/conformance/performance.md`; the untested control-graph lever is fenced
  by the M3 TAGBODY/GO contract.

- P3 async **cancellation** is exercised end to end by `p3/cancel-subtask`
  (3 repeats, both backends). Getting there fixed a real defect:
  `%canon-lower-async-host-func` packed its STARTED/RETURNED state under the
  callee task's mutex, but a HOST-PENDING callee deliberately has no
  `async-task` record, so the pack dereferenced NIL (TYPE-ERROR inside
  `%CANON-LIFT-ASYNC`). The host-pending arm now packs without the mutex
  (its resolution is delivered later by the pump, so there is no second
  reader to race). Encoding note: lowering an async function needs the
  `async` canon option — it flattens to `(i64)->i32`; without it the lower
  flattens to `(i64)->()` and fails the core-import signature check.
- **Generated upstream-toolchain guest (2026-11-02).** The pinned Rust
  `nightly-2026-11-01` + `wasm32-wasip3` toolchain builds
  `tests/toolchain/wasi-guest`; Rust std labels the 0.3 line `@0.3.0`, so the
  reproducible build normalizes those same-length interface labels to the
  vendored `@0.3.2` release and runs `wasm-tools validate`. The tracked
  artifact (`tests/fixtures/wasi/toolchain-guest.wasm`, hash in
  `TOOLCHAIN-CHECKSUMS`) runs with exact stdout in **both** backends via
  `make test-toolchain-guest`. The hand-authored P3 commands remain under
  `tools/build-p3-fixtures.lisp`/`P3-CHECKSUMS`; the upstream precompiled
  wasip3 suite is still green at 127/0/0.
- **Upstream `wasi-testsuite` lane (2026-11-02, F1): registered, measured,
  GREEN at the pinned commit.** `adapters/wasmdown.py` +
  `tools/run-wasi-testsuite.py` (`make test-wasi-upstream`) run the
  precompiled guests at the commit pinned in `specs/lock.toml
  [suite.wasi-testsuite]` (`9d84631e427729802085c3d172d0a63cd360a1fa`,
  `prod/testsuite-base`; the driver refuses any other HEAD) and write
  `tests/wasi-upstream-results.json` + `tests/wasi-upstream-summary.md`.
  Measured over all 127 pinned guests: **127 pass / 0 fail / 0 skip**
  (28 / 67 when first measured; 72 / 23, then 95 / 32, before HTTP and
  sockets were admitted; the lane now runs the full pinned set with no
  exclusion expectations). `wasm32-wasip3` is **clean: 55 pass / 0 fail /
  0 skip**: a component may declare BOTH release lines,
  and the graph composes its 0.3-line imports from the 0.3 command builders
  with its exact-version 0.2 imports from the P2 tables under one
  `with-args` alist (HTTP and 0.3 sockets admitted, `@unstable` gating and
  the P2 version-exact selection remain in force; no provider is invented).
  The newly admitted tests exposed host-contract defects, now fixed and
  pinned: CLI argv[0] is the guest program's file name; an empty path is
  `no-entry`; a directory's `get-flags` never reports the file-only `write`
  and reports `read`; `advise` on a directory is `bad-descriptor`;
  `set-size` maps a size or offset beyond `off_t` to `file-too-large` /
  `invalid`; `stat` reports atime, mtime and ctime separately; the
  `insecure-seed` provider is stable per instance; a zero-length read of a
  host-backed stream completes immediately; a `pread`/`pwrite` offset above
  2^63-1 is `invalid`; and a component exporting several `wasi:cli/run`
  versions is driven through the newest. `wasm32-wasip1` is also **clean:
  72 pass / 0 fail / 0 skip** (AssemblyScript 12/12, C 14/14,
  Rust 46/46), after the runtime fixed the preopen base/inheriting rights
  split (a directory base no longer advertises `fd_write`, so an
  `O_DIRECTORY` open is not forced to `O_RDWR`), handle-relative path
  resolution, directory-fd rights, `fd_readdir` paging/`.`-inode, ns
  timestamps, the `poll_oneoff` subscription stride and revents, and
  `AT_SYMLINK_*` flag misuse in `linkat`/`utimensat`, and the
  `sock_shutdown` filetype check (ENOTSOCK). The network-only subset runs as
  its own lane: `tools/setup-network-tests.sh` fetches the pinned tree and a
  reference Wasmtime into `.cache/`, `make test-wasi-network` runs the
  admitted http/sockets cases through `tools/run-wasi-network-tests.py`
  (30/30), and `make test-wasi-network-reference` runs the same subset
  through the upstream Wasmtime adapter (28/30; two known Wasmtime-50
  failures, report-only). Neither lane is a release gate.
- **Reactor liveness is deadline-based, by design.** The drain oracle treats
  an armed one-shot clock deadline (plus pending host work) as possible
  progress, so a `wait-for`/`wait-until` longer than any fixed budget
  completes; a bare fd-readiness watch is excluded, because a peer may never
  become ready, and is therefore not a liveness guarantee. Genuine deadlocks
  still trap: the component lane's `component/deadlock.wast` and
  `component/dont-block-start.wast` assert the deadlock trap and stay 2/2
  green — the negative control for the predicate.
  Pinned by `wasi/reactor-pending-deadline-p`; end-to-end proof is
  `p3/deadline-wait` (200 ms).

## Roadmap

**Providers.** The CLI owns its reactor now (clock deadlines and stream
readiness work without an embedder); what remains is keeping every blocking
host path (blocked stdio) on the bounded executors rather than a caller's
thread. The network surface is implemented: HTTP `types`/`client`/`handler`/
`service` and sockets `types`/`ip-name-lookup` under the declared authority;
the middleware world is composed by `http-middleware-providers` with an embedder-supplied next handler; no upstream middleware fixture exists.

**Canonical ABI.** The position-dependent `stream`/`future` arms are gone; the
`borrow` lend arms that remain in the two canon-lower wrappers are
load-bearing, not vestigial: an async-typed callee resolves a borrow index in
its own table, so the raw caller index is not substitutable (the regressing
rows are `passing-resources::1` and `drop-cross-task-borrow::2/::4/::6`).
Live-borrow accounting traps `task.return`/`task.cancel` with outstanding
borrows, compound `result` layouts are pinned, and the former
lend/destructor reclamation defect is resolved: host temporary borrows are
reclaimed exactly once and store metadata retires with the store (see Known
gaps). Nested `option<stream<u8>>` ownership is likewise resolved.

**Backends.** The pinned core parity lane, the component compiled report and
the cost bench exist; what remains is differential fuzz/property lanes and the
remaining CI wiring (see [`conformance/matrix.md`](conformance/matrix.md)).
The compiled tier is correctness-first; unboxing i64 and removing the extra
`funcall` on compiled-to-guest edges are performance work, not scope.

**WASI.** Finish the 0.1 and 0.2 facades; the 0.2 registry already binds 29 of
31 stable interfaces at exact release names: 29 bound imports plus the two
export-only entries (`wasi:cli/run`, `wasi:http/incoming-handler`), with HTTP
and sockets implemented on both lines.

The engineering record — how each of these arrived, and the measurements that
drove the decisions — is in [`history.md`](history.md); the lane measurements
quoted here are regenerated by the lanes documented in
[`conformance/`](conformance/README.md).
