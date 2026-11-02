# WASI

Two ABIs share the same native authorities: the core-module
**Preview 1** linear-memory ABI (`wasi_snapshot_preview1`), and the
component-model providers for **0.3.2** plus a **0.2** release-line
facade. Nothing reaches the host except through a capability.

## Capability model

Every host import receives an explicitly built authority object — a
`wasi-context`, `wasi-preopen`, or `wasi-network` — never an ambient global.

1. **No ambient authority.** An absent stream traps `host-error`; a
   context with no network authority denies every socket operation. In the
   CLI, `--dir`, `--env`, `--net`, `--listen`, `--dns` and guest arguments
   require `--wasi`. Network authority exists only as far as it is declared:
   `--net HOST:PORT` (outbound), `--listen ADDR:PORT` (bind) and `--dns NAME`
   (resolution) are repeatable, `*` is a wildcard, and with none declared the
   context carries no network at all (see below).
2. **Handle-relative filesystem, no path-prefix filter.** A preopen is
   an already-open host directory handle plus its guest-visible name.
   Every file operation is an `*at` call against a descriptor: there is
   no string prefix to bypass. `close-wasi-preopen` releases the
   handle and invalidates the cached descriptor.
3. **The kernel never follows a symlink for us.** Path resolution
   walks components one at a time with `O_NOFOLLOW` and expands links
   itself with `readlinkat` under a 32-link budget. A link whose target
   is absolute or contains `..` is refused; `..` components, NUL, and
   empty paths are refused outright. A link swapped in mid-walk can
   only make the operation fail, never escape.
4. **No cross-authority moves.** A descriptor is checked against the
   calling context's preopen list, and a socket against the network it
   was created from; a foreign handle is rejected rather than used.
5. **Preopens are non-owning.** A guest `resource.drop` of a preopen
   handle never closes the root; finalizers close only fds the runtime
   opened for itself.
6. **Network policy is consulted per operation** — bind, connect, UDP
   send and name resolution — and refusal rides the interface's own
   `access-denied` error. Guest-declared `wasi:sockets` imports bind under
   this authority (see `wasi:sockets` below).

## Configuring capabilities

| Capability | CLI | Lisp |
| --- | --- | --- |
| Enable the WASI set | `--wasi` | `make-wasi-context` |
| Preopen | `--dir HOST[::GUEST][:ro]` | `open-wasi-preopen` in `:preopens` |
| Environment | `--env K=V` (repeatable) | `:environment` alist |
| Guest arguments | tokens after `--` | `:arguments` (the CLI sets the first to the input file's basename, POSIX `argv[0]`) |
| Standard streams | implied by `--wasi` | `:stdin` `:stdout` `:stderr` (NIL = withdrawn) |
| Network (TCP/UDP/DNS) | `--net HOST:PORT`, `--listen ADDR:PORT`, `--dns NAME` (repeatable; `*` wildcard; require `--wasi`) | `:networks` (`make-wasi-network` with `:outbound` / `:listen` / `:dns`) |
| HTTP transports | built-in plain TCP serves `http`; `--http-transport` is refused (embedder-only) | `:http-transports` (`wasi-http-transport` CL objects); https is `configuration-error` without one |
| Reactor | built by `--wasi` (serves clock deadlines and stream readiness) | `:reactor` |

A preopen's authority is the permission set the embedder granted —
`"read"`, `"write"`, `"mutate-directory"` — from which the guest's
rights derive; `:ro` yields `("read")` and can never be widened. The
default is all three.

## 0.3.2 provider coverage

Providers are bound from the vendored v0.3.2 WIT snapshot; version
dispatch is by the `@0.3.2` suffix of the import name.

| Package | Delivered |
| --- | --- |
| `wasi:clocks` | `monotonic-clock` `now` / `get-resolution`, `system-clock` `now` / `get-resolution`, `types.duration`. The async `wait-until` / `wait-for` park on the context's reactor; the CLI builds that reactor itself (`--wasi`), so a parked deadline **completion** is servable through the CLI — `p3/deadline-wait` proves a 200 ms `wait-for` returns only after the requested time. An embedder that supplies `:reactor` gets the same behaviour. `clocks-timezone` is `@unstable` and is refused at link time (see “Stability gating”). |
| `wasi:random` | `get-random-bytes`, `get-random-u64`, and the insecure `get-insecure-random-bytes` / `-u64` / `get-insecure-seed` — all from OS entropy, never the Lisp RNG. Zero-length requests return an empty list; a single request is capped at 1 MiB. |
| `wasi:cli` | `environment` (`get-environment`, `get-arguments`, `get-initial-cwd`, lowered as `list<string>`/`list<tuple<string,string>>` through guest memory + `realloc`), `exit` (`exit`, `exit-with-code` via the `wasi-exit` condition), `stdin`/`stdout`/`stderr` as real streams, the `terminal-*` interfaces (`isatty` answers `some` or `none`, never a fabricated handle), and the command world's `run`. Guest evidence: `p3/cli-stdin-env-stderr-terminal` asserts argv[0] and an extra argument, a `--env` pair and the option-typed initial cwd, sees `terminal-stdout` as `none` on a captured (non-tty) stdout, reads real stdin (`stdin-probe\n`) through `read-via-stream` requiring the explicit DROPPED EOF result and an ok paired future, and writes to stderr through `write-via-stream`; `p3/import-exit` covers `exit`, `p3/stream-stdout` covers stdout. |
| `wasi:filesystem` | The `descriptor` resource (own/borrow, destructors, non-owning preopens), all non-stream descriptor methods with the errno → `error-code` table, `preopens.get-directories`, `read-via-stream` / `write-via-stream` / `append-via-stream` / `read-directory`, and flag gating that reports `not-permitted` / `read-only` through the interface's `result` rather than throwing. Guest evidence: `p3/filesystem-preopens-read` lists a `/work` preopen, opens a seeded `hello.txt` through it with an async `open-at` (waited as a subtask), reads exactly 17 bytes with `read-via-stream` + `stream.read`, gets `error-code::no-entry` as an err result (not a trap) for a missing path, creates `out.txt`, writes at explicit offsets and appends at EOF (`stat` regular-file/link-count 1/size 22, byte-exact read-back), gets `error-code::read-only` (case 32) as an err result for a write through the read-only descriptor, and exercises `create-directory-at` / `stat-at` / `read-directory` (enumerating `hello.txt`, `out.txt`, `subdir`, `temp.txt`) / `rename-at` / `unlink-file-at` / `remove-directory-at`, with a host-side witness that the removed objects are absent. Link/symlink operations and the stat symlink-follow policy, the advanced metadata/time calls (`advise`, `sync`, `sync-data`, `set-size`, `set-times`, `get-flags`, `get-type`, `metadata-hash`, `is-same-object`) and stat timestamps are provider/unit-tested but not guest-evidenced; there is no seek API in 0.3.2. |
| `wasi:sockets` | **Supported under the declared authority.** `types` provides `error-code`, the address types and the `tcp-socket`/`udp-socket` resources; `ip-name-lookup` provides `resolve-addresses` (DNS). Both bind for guest-declared 0.3-line imports, and every bind, connect, UDP send and resolve is checked against the context's `wasi-network` (CLI `--net`/`--listen`/`--dns`, or `make-wasi-network`). Undeclared destinations are `access-denied`; with no authority every operation is refused. Readiness is a bounded 2 s poll, not a socket reactor. Evidence: the pinned upstream http/sockets subset is 30/30 (`make test-wasi-network`). |
| `wasi:http` | **Supported (0.3.2).** `types` provides the `fields`, `request`, `request-options` and `response` resources with header/body/trailer semantics; `client.send` speaks HTTP/1.1 on the built-in plain TCP transport, including streaming request uploads and chunked response trailers. The incoming `handler` interface and the `service` world are served by `serve-component` / `wasmdown serve`, which answers requests through the exported handler until SIGINT. A `https` request needs an EMBEDDER-registered `wasi-http-transport`; without one `send` returns `configuration-error` (never a plaintext fallback, and the runtime implements no TLS itself). The `middleware` world's host-supplied `next`-handler composition is implemented by `http-middleware-providers` (embedder API; unit-pinned, no upstream middleware fixture). On the WASI 0.2 (Preview2) legacy line there is still no `wasi:http` provider; the declared HTTP surface is the 0.3.2 one.

The 0.2 line serves the **outgoing** HTTP surface: `wasi:http/types@0.2.x`
(all resource types and methods: `fields`, `incoming-request`,
`outgoing-request`, `request-options`, `response-outparam`,
`incoming-response`, `incoming-body`, `future-trailers`, `outgoing-response`,
`outgoing-body`, `future-incoming-response`, `http-error-code`) and
`wasi:http/outgoing-handler@0.2.x.handle` are bound at the exact declared
patch version (`0.2.0` … `0.2.12`). Requests go through the same HTTP/1.1
transport and `wasi-network` authority as the 0.3 line; bodies are P2
`input-stream`/`output-stream` resources and the two futures expose
`subscribe` pollables. The **incoming** side is served too:
`wasi:http/incoming-handler@0.2.x.handle` is a synchronous export, so a 0.2
proxy-world component is instantiated with the exact-version P2 providers
(io/clocks/random/cli/stdout/stderr/stdin + http/types +
http/outgoing-handler) and driven by `wasmdown serve`. The host buffers the
HTTP/1.1 request (Content-Length or chunked, trailers included), builds the
`incoming-request` resource, calls `handle(request, response-outparam)`,
captures `response-outparam.set`, then serializes the captured
`outgoing-response` status, headers, body and trailers. An unset outparam or
a handler trap answers 500. The inventory records `incoming-handler` as an
export-only interface (it is a world export, never an import provider).

## Serving HTTP

`serve` dispatches on the component's exports: a service-world component
exporting `wasi:http/handler` runs the 0.3 path, while a proxy-world
component exporting `wasi:http/incoming-handler@0.2.x` runs the 0.2 path
(`serve-preview2-component`). Both serve HTTP/1.1 until SIGINT, and the URL
is the **first stderr line**, before any guest output:

```sh
sbcl --script tools/wasmdown.lisp serve service.wasm \
  --wasi --addr 127.0.0.1:8080 --dir .::/work
# stderr: http://127.0.0.1:8080
```

`--addr` defaults to `127.0.0.1:0`, so the printed URL carries the
kernel-assigned port. The same capability flags as `run` apply (`--dir`,
`--env`, `--net`, `--listen`, `--dns`); a request body is buffered before the
guest handler call, and the response status, headers and buffered body are
serialized from the returned `response` resource (the body is read from the
response's contents stream after its writer drops it). On the 0.2 path the
response body is collected from the `outgoing-body` output-stream and
trailers from `outgoing-body.finish`; a non-empty trailer section switches
the reply to chunked encoding, and request chunked trailers are exposed
through `incoming-body.finish`'s `future-trailers`. `--http-transport` is
refused: TLS transports are embedder-only (`:http-transports` on the
`wasi-context`).

Coverage is enforced at construction: an unimplemented function of a
bound interface is a `host-error`, never a trapping stub. Provider values are
native Lisp shapes: a payload-less variant case is a bare label (for example
`"no-entry"`), while a payload-bearing case must be tagged
`(label . payload)`. A bare label for a payload-bearing case is a
`host-error` (`a bare variant label (...) names a case with a payload; pass
(label . payload)`), never a discriminant-only half-value; pinned by
`component/abi-bare-variant-label`.

### Stability gating

`@unstable` is enforced, not just recorded. On the 0.3 path the runtime
checks every `wasi:` import a component **declares** against the WIT
selection surface before composing anything; if the target interface carries
`@unstable(feature = …)` and that feature is not enabled, the run is a
link error naming the import and the feature:

```
wasi:clocks/timezone@0.3.2 is @unstable(feature = clocks-timezone) and that feature is not enabled; unstable interfaces are outside the selected stable WASI 0.3.2 surface (see docs/usage/wasi.md and specs/wasi/MANIFEST.md).
```

There is **no CLI flag that enables a feature today**, so the selected
surface is the stable one and the gate is refusal-only. Stable imports
are unaffected; the pinned `wasi:cli/command` world's own composition is
not a declared guest import and is not gated by this check.

## Declared limits

| Limit | Effect |
| --- | --- |
| Reactor liveness | The CLI builds a reactor, so `clocks.wait-until` / `wait-for` complete there instead of signalling `host-error`; an embedder may supply `:reactor` directly. An armed one-shot deadline counts as possible progress for the drain oracle, but a bare fd-readiness watch does not — a peer may never become ready — so readiness alone is not a liveness guarantee. Genuine deadlocks still trap. |
| `@unstable` gating | Enforced at link time on the 0.3 path: a declared import of a gated interface whose feature is not enabled is a link error naming the import and the feature (see “Stability gating”). No CLI flag enables a feature, so unstable interfaces stay outside the selected profile. |
| `proc_raise` (Preview 1) | A valid signal returns `errno::notsup`; an invalid one `errno::inval`. The hosting SBCL process is never signalled. |
| Resource lifetime | Descriptor fds are reference-counted (the handle plus every stream it handed out) and close exactly once at zero references; six open/drop cycles keep the process open-fd count at baseline. A host-called lower's temporary borrow handle is reclaimed when the call finishes (or at a host-pending's terminal outcome), and freed table slots are reused. Component graph metadata (destructor and provenance tables) retires when the owning store is torn down; a long-lived caller-owned store reused for many top-level instantiations **without** teardown still accumulates those rows. |

## WASI Preview 1

`src/wasi-preview1.lisp` implements the full 46-import
`wasi_snapshot_preview1` surface from the pinned WITX snapshot, whose
census in `specs/wasi/preview1/INVENTORY.md` is the specification.

- **Memory before start.** The guest memory is bound in the
  `before-start` hook, so a start section that calls `fd_write`
  already sees its exported memory and active data segments.
- **Rights model.** The fd table maps descriptors to explicit
  capabilities with flags and positions; a read-only preopen denies
  creation, writing, and stronger inheriting rights.
- **Path confinement.** Resolution is handle-relative and `..`-confined:
  an internal `a/../b` stays inside the root, while `..` at the root,
  absolute guest paths, and links leaving the root are `NOTCAPABLE`.
- **Bounds before access.** Every pointer and count is validated as a
  Lisp integer first and reported as `FAULT` / `INVAL` / `OVERFLOW`.
- **Exit.** `proc_exit` unwinds through `wasi-exit` and never calls
  `exit(3)`; `poll_oneoff` normalises each subscription against its own
  clock and answers in subscription order.

Run it with `run <module> --wasi`, or `wasmdown.wasi:run-preview1`.

## WASI 0.2 facade

A component importing exact `@0.2.x` release names gets the versioned
0.2 provider table; anything else gets the vendored 0.3 command world.
The choice follows the component's **declared imports**, never a file
name. A component may declare **both** release lines: the mixed graph
composes its 0.3-line imports from the 0.3 builders and its exact-version
0.2 imports from the P2 tables under one `with-args` alist (this is what the
upstream wasip3 fixtures do). The 0.2 line remains a legacy compatibility
target: it has no `wasi:http` provider, and its `wasi:sockets/*` bindings are
legacy coverage, not the declared network surface.

`src/wasi-preview2.lisp` binds 29 of the 31 stable interfaces at their
exact release names. The pinned SQLite component for this line decodes,
validates, instantiates and executes real SQL through
`wasi:cli/run@0.2.12`, in memory and on disk.

Measured behaviour for both ABIs:
[`../conformance/wasi.md`](../conformance/wasi.md).
