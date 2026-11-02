# Backend × ABI matrix

Two guest backends are selectable and explicit — `:interpret` (the reference)
and `:compiled` (every defined function compiled before the start section) —
and the same host providers serve both. This page records, per surface, what is
measured and where the numbers live. It is hand-written; the lane artifacts it
links are generated.

## Measured matrix

| Surface | `:interpret` | `:compiled` | Evidence |
| --- | --- | --- | --- |
| Pinned core suite (97 scripts, 21 260 commands) | 21 187 pass / 0 fail / 73 unsupported | 21 260 / 21 260 commands agree in outcome and phase; 0 compile refusals | [`core.md`](core.md), [`backends.md`](backends.md), `make test-backend-parity` |
| Vendored component suite (1 702 commands) | 1 702 / 0 / 0 / 0 — released gate | 1 702 / 0 / 0 / 0 — same denominator | [`component.md`](component.md), `tests/component-compiled/component.md` (`make report-component-compiled`) |
| WASI Preview 1 acceptance (in-memory and on-disk SQLite) | 14/14 checks pass | 14/14 checks pass: smoke, persistence (two processes), reopen, rollback, bail, no-preopen, read-only, escape — 821 s of compile-heavy wall time. The lane asserts exact stdout/status per case, so both backends meet identical expectations | `tools/wasi-preview1-acceptance.py [--backend compiled]` |
| WASI 0.2 behaviour lane (11 cases) | 11/11 pass | 11/11 pass (11 x ~200 s compile wall time). Each case's manifest entry asserts exact stdout bytes and exact status, so both backends satisfy the SAME byte-level expectations — stronger than a post-hoc sha diff. Per-case records live under `tests/wasi-results/` (`latest/` for interpret, the compiled run's `--output-dir`), distinguished by the `backend` field | `tools/run-wasi-suite.py --abi preview2 [--backend compiled]` |
| WASI 0.3.2 providers | unit/provider pins, component async suite | same providers; component suite green under `:compiled` | [`inventory.md`](inventory.md), `tests/wasi*.lisp` |
| Upstream `wasi-testsuite` (pinned `9d84631e`, 127 precompiled guests) | **green: 127 pass / 0 fail / 0 skip.** `wasm32-wasip3`: 55 pass / 0 fail / 0 skip — a component that declares BOTH release lines is admitted by composing its 0.3-line declared imports from the 0.3 command builders with its exact-version 0.2 imports from the P2 tables (HTTP and 0.3 sockets admitted; `@unstable` still gated; no provider invented). `wasm32-wasip1`: 72 pass / 0 fail / 0 skip (AssemblyScript 12, C 14, Rust 46) | not measured; the adapter runs the CLI's default backend, and the admitted tests exercise host providers, not codegen | `tests/wasi-upstream-summary.md`, `tests/wasi-upstream-results.json`, `make test-wasi-upstream` (checkout not vendored) |
| `wasm32-wasip1` declared profile (46 imports incl. legacy `sock_*`) | Same measured lane above: 72 pass / 0 fail / 0 skip; the two `sock_shutdown-*` C tests pass after the ENOTSOCK-from-filetype fix | same | `tests/wasi-upstream-results.json`, `docs/status.md` |
| Upstream network subset (`http` + `sockets` proposals) | **30/30** through `tools/run-wasi-network-tests.py` (`tools/setup-network-tests.sh` then `make test-wasi-network`); the same subset through upstream Wasmtime is a 28/30 report-only smoke (two known Wasmtime-50 failures) | same adapter/CLI default backend | stdout summary from the runner, `make test-wasi-network`, `make test-wasi-network-reference` |
| Async property lane (task/subtask, stream/future/cancel, host-pending) | 14,071 checks / 0 failures over 8 fixed seeds x3 (`make test-async-property`); negative control detects an injected double delivery (`make test-async-property-negative`) | same host-side lane (backend-independent: the scheduler/executor is shared by both guest backends) | stdout; `tests/async-property.lisp` |
| Blocked-stdio / DNS executor audit | fixed 2-worker budgets, worker-only execution and affinity, max active <= budget, close drops queued jobs and refuses new submissions without joining, `close-wasi-context` rejects/detaches; guest stdio green in both backends | same (host-side) | `tests/async-property.lisp`, `tests/wasi.lisp`; P3 lanes |
| 0.3.2 sockets provider (`types` + `ip-name-lookup`) | Supported under the declared `wasi-network` authority; upstream network subset green (30/30), unit pins incl. `wasi/e2e-tcp-echo-component` PASS | same test binary, host listener + echo thread | `make test-unit`; `tests/wasi.lisp`; `make test-wasi-network` |
| 0.3.2 HTTP (`types` + `client`; `handler`/`service`) | Supported: outgoing `client.send` over HTTP/1.1 plus the incoming handler served by `wasmdown serve`; upstream HTTP cases are part of the 127/0/0 lane. TLS is embedder-only (`:http-transports`); `middleware` host-supplied-next composition is unit-pinned (no upstream fixture) | same host-side providers | `make test-wasi-upstream`; `make test-wasi-network`; `tests/wasi.lisp` |
| 0.2 HTTP incoming (`incoming-handler@0.2.x`, proxy world) | Supported: `wasmdown serve` detects a component exporting `wasi:http/incoming-handler`, composes the proxy-world providers at the guest's exact 0.2 patch version(s), buffers HTTP/1.1 request bodies (Content-Length or chunked, request trailers included), calls the sync `handle(request, response-outparam)`, and serializes the captured `outgoing-response` status/headers/body/trailers; an unset outparam or trap answers 500. Host loopback plus provider and response-outparam pins: `tests/wasi-preview2.lisp` | same host-side provider path | `make test-unit` (P2 lane), `tests/wasi-preview2.lisp` |
| Pinned 0.3.2 guests (hand-authored) | 10/10 pass ×3 repeats (30/30): no-import success smoke, a `wasi:cli/exit@0.3.2` importer (exit 7), a `wasi:clocks/monotonic-clock@0.3.2` importer (monotonic `now()`), a resume-safe async command (callback `task.return`), a `write-via-stream` stdout command (`hi`), a cancelled-subtask command, a `wasi:random/random@0.3.2` guest (`insecure`/`insecure-seed`: exact 16-byte lengths, differing secure reads, `u64`/seed calls; `realloc`-lowered lists), a clock-deadline guest (`wait-for` 200 ms completes through the CLI reactor; the 200 ms bound is the liveness-oracle regression proof), a `wasi:cli/{stdin,environment,stderr,terminal-stdout}@0.3.2` guest (argv[0]/marker, `--env` pair, option-typed initial cwd, `terminal-stdout` = `none` on a non-tty, real stdin `stdin-probe\n` with explicit DROPPED EOF and an ok paired future, stderr through `write-via-stream`), and a `wasi:filesystem/{types,preopens}@0.3.2` guest (`/work` preopen, async `open-at` of seeded `hello.txt`, exactly 17 bytes through `read-via-stream`+`stream.read`, writes at explicit offsets and appends at EOF with `stat` and a byte-exact read-back, err `read-only` (case 32), `create-directory-at`/`stat-at`/`read-directory` enumeration/`rename-at`/`unlink-file-at`/`remove-directory-at`, missing path = err `no-entry`, every handle dropped) | 10/10 pass, same cases and repeats (30/30); the nested core runs compiled (`backend-entry-count`) | `make test-wasi-preview3`, `tools/build-p3-fixtures.lisp`, `wasi/command-component-runs-the-selected-backend`, `wasi/reactor-pending-deadline-p` |

Network communication (TCP, UDP and DNS) and `wasi:http` are **part of the
declared support profile**: guest-declared 0.3-line sockets imports bind under
the `wasi-network` authority (`--net`/`--listen`/`--dns` or the embedder API),
HTTP `types`/`client` are served, and `handler`/`service` are exercised by
`wasmdown serve`. The upstream network subset is the evidence row (30/30); the
`middleware` world host-supplied `next` composition is implemented by `http-middleware-providers` and unit-pinned (no upstream fixture)
by the pinned suite and is not claimed. See
[`../../specs/wasi/MANIFEST.md`](../../specs/wasi/MANIFEST.md) § “Network
surface (declared supported)”.

The backend property lives on the function, not on a replacement object: the
selected mode compiles each defined function's existing `func-instance`
(`backend`, `compiled-entry`), so exports, tables, element segments, `ref.func`
and other modules' imports keep the same reference, and an imported function
keeps the backend of the instance that defined it. A compile refusal is
`wasmdown:codegen-error` (function, byte offset, opcode) raised before active
segments and before the start section; there is no interpreter fallback.

## Resource behaviour

- Every host call re-resolves the guest memory, so `memory.grow` between two
  imports is visible to both backends (`tests/wasi-preview1.lisp`
  `memory-grow-between-host-calls`, run in both modes).
- Descriptor release is asserted on success, trap and `proc_exit` paths, and a
  rejected `path_open` out-pointer releases the fd it had opened; the P2 audit
  pins the accept/stream reference-count arithmetic (`tcp-accept-does-not-leak-descriptors`).
- Component handles reuse their free-list slot; the async suite's
  `drop-cross-task-borrow` and `passing-resources` fixtures exercise
  cross-task borrow accounting in both backends.

## Cost

[`performance.md`](performance.md) (`make report-backends-bench`) separates
decode / validate / instantiate(+compile) / run per backend. On the synthetic
kernels `:compiled` runs 7–17x faster than the interpreter, and eager
compilation costs 0.4–1.5 ms and 0.3–1.3 MiB per small module. A 6 MB guest
(SQLite, 2 039 defined functions) needs ~8 GiB of SBCL heap and ~200 s to
compile — about 98 ms per function — which is why WASI compiled runs are
report evidence rather than a gate. The bench table carries each fixture's
function count so compile cost is readable per function.

## Release check

`make verify-release` (`tools/check-release-matrix.py`) reads the recorded
artifacts and requires every row above to be present, green, and to carry
a tested source commit that is HEAD or an ancestor of it (the artifacts are
committed after the run, so a row may record an earlier commit): core parity
summary, component records for both backends, the P2/P3 WASI records, the
bench JSON and the inventory. Each row prints its own source, and the
summary lists every distinct tested commit, so a lagging row (typically the
~37 min compiled P2 lane) is visible rather than averaged away. P1 is
stdout-only and is listed as `n/a` with `make verify` named as its gate.

## Resolved

- **Resume-safe async P3 guest.** The lane pins resume-safe commands:
  `p3/async-callback` keeps continuation state in memory, `p3/stream-stdout`
  parks the future read and waits on real events, and `p3/deadline-wait`
  parks on a reactor deadline. The four-instruction fixture in
  `tests/wasi.lisp` remains non-resume-safe and is retained only as the
  in-process `run-component` case (recorded in `docs/status.md`).
  `tools/run-wasi-suite.py --abi preview3` selects the 0.3.2 ABI explicitly
  and FAILS on a missing fixture (never skips); `--abi all` covers the two
  released ABIs only.
- **`values/post-return.wast`** is no longer excluded: post-return isolation
  is implemented, the file passes 71/71, and the component lane denominator
  moved from 1,631 to 1,702 commands.
- **Deadline completion, cli stdio/env/stderr/terminal, and the filesystem
  read/write/directory/error paths.**
  `p3/deadline-wait` (200 ms `wait-for`, and the liveness-oracle fix that
  lets an armed reactor deadline count as possible progress while a bare
  fd-readiness watch does not), `p3/cli-stdin-env-stderr-terminal` (argv/env/
  initial-cwd, `terminal-stdout`, real stdin `stdin-probe\n` with the
  explicit DROPPED EOF and an ok paired future, stderr `write-via-stream`)
  and `p3/filesystem-preopens-read` (the `/work` preopen, async `open-at` of
  the seeded `hello.txt`, exactly 17 bytes through `read-via-stream` +
  `stream.read`, a missing path as err `no-entry`, every handle dropped)
  joined the P3 lane, now 10/10 ×3 per backend. The filesystem guest also
  exercises `write-via-stream` at explicit offsets (including zero-fill
  extension) and `append-via-stream` at EOF, parked write futures, `stat`
  (regular-file, link-count 1, size 22), a same-descriptor read-back and a
  host-side file-contents witness. A write through the read-only
  `hello.txt` descriptor resolves err `error-code::read-only` (case 32)
  through the paired future, never a trap. The directory surface is
  guest-evidenced on the same preopen: `create-directory-at`, `open-at` of
  the directory, `stat` reporting `directory` (case 2), `read-directory`
  enumerating `hello.txt`/`out.txt`/`subdir`/`temp.txt` with an ok future,
  `rename-at` → `moved.txt` followed by `stat-at` (regular-file, size 0),
  `unlink-file-at`, `remove-directory-at`, and `stat-at(subdir)` err
  `no-entry`; the suite adds a host-side `absent` witness. The guest was
  then extended to the FULL pinned surface: `link-at`, `symlink-at`/
  `readlink-at` and stat with/without symlink follow, `advise`, `set-size`,
  `set-times`/`-at`, `get-flags`, `get-type`, `sync`/`sync-data`,
  `metadata-hash`/`-at` and `is-same-object`, with err-case witnesses. The
  deadline, cli and filesystem rows are closed.
- **Nested `option<stream<u8>>` payload ownership (2026-11-02).** `%abi-load`
  implements the loading spec's `stream`/`future` rows through
  `%lift-async-end`: a memory slot indexes the **loading (caller) instance's**
  table, the readable end is removed from that table with owner and index
  cleared, and the source slot is freed for reuse — a second load of the same
  index traps as stale. `%lift-async-end` enforces `lift_async_value`'s
  guards: readable class must match the valtype, element type must match
  through `%abi-type-key` (both type spaces resolved; own/borrow compare class
  only, because resource indices are alias-projected), and owner+index are
  both cleared. The async result-store context carries `:type-inst`, the
  **provider's** type space, so a nested `(:idx . n)` resolves there instead
  of the caller's space (measured with a colliding `u32` at caller index 0).
  Pinned by `component/indirect-async-payload-ownership-pin` — a
  forced-indirect sync host lower at 18 flat slots, async
  `option<stream<u8>>` results with colliding type indices, `none` allocating
  nothing, and stale/class/element/writable/non-idle/waitable-set traps — and
  by `component/generic-path-five-cases-pin` CASE 2, migrated to the
  end-object contract (the second load traps). Negative controls showed both
  the load arm and the guard set are load-bearing. Unit 739/0, component
  1,702/0, P3 10/10 ×3 per backend.
- **Host borrow reclamation and store-retirement metadata (2026-11-02).**
  A host callee has no Task to adopt the borrow handles a lowering staged, so
  the staged records were dropped and their slots leaked. Each staged handle
  is now a `pending-borrow` record (owner table, 1-based slot, handle)
  reclaimed exactly once: on the immediate return and on a trap, and across a
  HOST-PENDING only at the pending's terminal outcome (resolve, reject, or an
  acknowledged cancel — an un-acknowledged cancel keeps the borrow live).
  Reclamation clears a slot only while it still holds the same record, so a
  reused slot is never disturbed. `%handle-add` uses the shared Table free
  list, so repeated new/drop cycles plateau, and `%store-teardown` clears
  `resource-dtor-table`, `ct-defining-inst` and `comp-closure` for the store
  it names and the bound store, because every store-lifecycle caller discards
  the store afterwards. Pinned by `component/host-borrow-reclaim-pin`,
  `component/host-borrow-retained-across-pending-pin`,
  `component/store-metadata-retires-with-store-pin` and
  `component/resource-metadata-live-graph-isolation-pin` (warm-up plus four
  instantiate→new/drop→teardown cycles give identical metadata counts; six
  destructor runs leave capacity 2 and free list 2; a second live handle of
  the same type keeps the dtor registered; two live stores never strip each
  other's provenance), plus `wasi/fs-descriptor-drop-fd-plateau-pin` (six
  open→stream→drop→stream-end cycles at baseline open-fd count). The
  `component/generic-path-five-cases-pin` CASE 3c/B now asserts the freed
  slot is empty and back on the free list instead of encoding the leak.
  Residual limits are recorded in `docs/status.md` and §9.3: teardown-based
  reclaim means a caller-owned store reused for many top-level instantiations
  without teardown still accumulates dtor/provenance rows; scheduler-state
  teardown still targets the bound store; non-terminal host pendings are not
  force-reclaimed; freed slots are class-polymorphic and the class guards
  trap.
- **Compound error-result lowering (2026-11-02) — audited, no discrepancy.**
  No reproducer exists against this tree: `%abi-lower`'s `:result` arm lowers
  to `variant(ok, error)` with the discriminant leading the join.
  `component/abi-compound-result-lowering` was extended with four shapes —
  both arms empty records, mixed-arity tuples, a variant carrying a record,
  and `option`/`option` — and passes unchanged. **No lowering code was
  changed**; this row was withdrawn as a defect. The separate validator
  type-use row is resolved below.
- **Validator type-use limits (2026-11-02) — resolved, owner-space
  resolution.** An imported-instance functype whose parameter or result is a
  compound type brought in by an outer alias used to remap its nested refs in
  the instance declarator space, so an unrelated same-numbered declarator
  type could win. `entry-at` now returns `(ct, owner-view)`: an outer alias
  switches to the owner selected by its de Bruijn count, an eq-bound export
  propagates the target's owner, and `%materialize-component-ct` materializes
  a ct against that owner's space (owner-qualified through nested outer
  aliases and eq bounds), while declarator-local entries still use the
  declarator remap. Pinned by
  `component/remap-outer-compound-functype-owner` — the exact diagnostic pair
  (decoy-first same-numbered type validates with no memory; the real `string`
  leaf still rejects with a memory requirement), shape checks (`tuple<u32>` vs
  `tuple<string>`) and result-position companions, and the count-2 positive/negative pair. Unit 739/0, component
  1,702/0, P3 10/10 ×3 per backend, `make verify` green.
- **Validator instance type-index space and encoder s33 valtype indices
  (2026-11-02).** Two engine correctness fixes exposed by the WIT-shaped
  filesystem fixture; neither is a §9.3 defect row. `%vis-check-type-use`
  pushed every import/export declarator inside an INSTANCE type, but
  Binary.md 274 makes only type-bound declarators introduce a TYPE index
  (COMPONENT types keep every declarator, Binary.md 415), so a later
  reference could name an unrelated same-numbered declarator; the walk now
  pushes type-bound declarators only for instances. In
  `tools/component-encode.lisp`, valtype type indices are signed s33 per
  Binary.md 252; the old `u32` wrote 64 as `40` (the FUNC constructor byte),
  so a component whose valtype reference reached 64 round-tripped malformed.
  The P3 filesystem fixture/lane (`p3/filesystem-preopens-read`, checksum
  `a3293778…`) is the regression witness: the previous fixture passed
  because its references happened to land on slots the validator accepted
  (it referenced no exported `descriptor-stat`/`directory-entry` slot from a
  later declarator), and its valtype indices stayed below 64, where `u32`
  and `s33` agree. Unit 739/0, component
  1,702/0, P3 30/30 ×3 both backends, `make verify` green.

## Still open

- No open test row. The upstream suite is green (127/0/0, including HTTP and
  sockets) and the network subset is 30/30; only the real-toolchain guest
  remains blocked (see Blocked).

## Blocked

- **A real-toolchain WASI 0.3.2 guest.** The lane runs a hand-authored
  `@0.3.2` command assembled from the pinned binary grammar
  (`tools/build-p3-fixtures.lisp`, bytes pinned by `P3-CHECKSUMS`); no
  toolchain emits the 0.3.2 component ABI here, so this is ABI coverage, not
  real-toolchain integration evidence.
