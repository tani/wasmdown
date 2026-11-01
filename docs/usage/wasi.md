# WASI

Two ABIs share the same native authorities: the core-module
**Preview 1** linear-memory ABI (`wasi_snapshot_preview1`), and the
component-model providers for **0.3.2** plus a **0.2** release-line
facade. Nothing reaches the host except through a capability.

## Capability model

Every host import receives an explicitly built authority object — a
`wasi-context`, `wasi-preopen`, or `wasi-network` — never an ambient global.

1. **No ambient authority.** An absent stream traps `host-error`; a
   context with no network authority denies every socket operation;
   in the CLI, `--dir`, `--env`, `--net`, `--listen`, `--dns` and guest
   arguments are refused unless `--wasi` is given.
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
   `access-denied` error.

## Configuring capabilities

| Capability | CLI | Lisp |
| --- | --- | --- |
| Enable the WASI set | `--wasi` | `make-wasi-context` |
| Preopen | `--dir HOST[::GUEST][:ro]` | `open-wasi-preopen` in `:preopens` |
| Environment | `--env K=V` (repeatable) | `:environment` alist |
| Guest arguments | tokens after `--` | `:arguments` (first is `argv[0]`) |
| Standard streams | implied by `--wasi` | `:stdin` `:stdout` `:stderr` (NIL = withdrawn) |
| Outbound network | `--net HOST:PORT` | `make-wasi-network :outbound` |
| Listen address | `--listen ADDR:PORT` | `:listen` |
| DNS policy | `--dns NAME` or `--dns *` | `:dns` (`:none`, `:all`, or a name list) |
| Reactor | none | `:reactor` |

A preopen's authority is the permission set the embedder granted —
`"read"`, `"write"`, `"mutate-directory"` — from which the guest's
rights derive; `:ro` yields `("read")` and can never be widened. The
default is all three.

## 0.3.2 provider coverage

Providers are bound from the vendored v0.3.2 WIT snapshot; version
dispatch is by the `@0.3.2` suffix of the import name.

| Package | Delivered |
| --- | --- |
| `wasi:clocks` | `monotonic-clock` `now` / `get-resolution`, `system-clock` `now` / `get-resolution`, `types.duration`. The async `wait-until` / `wait-for` park on the context's reactor. `clocks-timezone` is gated off (`@unstable`). |
| `wasi:random` | `get-random-bytes`, `get-random-u64`, and the insecure `get-insecure-random-bytes` / `-u64` / `get-insecure-seed` — all from OS entropy, never the Lisp RNG. Zero-length requests return an empty list; a single request is capped at 1 MiB. |
| `wasi:cli` | `environment` (`get-environment`, `get-arguments`, `get-initial-cwd`), `exit` (`exit`, `exit-with-code` via the `wasi-exit` condition), `stdin`/`stdout`/`stderr` as real streams, the `terminal-*` interfaces (`isatty` answers `some` or `none`, never a fabricated handle), and the command world's `run`. |
| `wasi:filesystem` | The `descriptor` resource (own/borrow, destructors, non-owning preopens), all non-stream descriptor methods with the errno → `error-code` table, `preopens.get-directories`, `read-via-stream` / `write-via-stream` / `append-via-stream` / `read-directory`, and flag gating that reports `not-permitted` / `read-only` through the interface's `result` rather than throwing. |
| `wasi:sockets` | The network authority, `tcp-socket` and `udp-socket` (create, bind, connect, listen, accept, send, receive, addresses, knobs), `ip-name-lookup` via `getaddrinfo`, and the errno → `sockets-error-code` table. A socket's handle plus its send/receive/listen streams share one reference count; the fd closes at zero. |
| `wasi:http` | **Unsupported by design — not an implementation gap, not planned.** Excluded permanently by user decision (2026-11-02); removed in M17 and recoverable from git history. There is no provider and no transport option. The vendored `specs/wasi/wasi-wit-0.3.2/http/*.wit` files remain reference data only. Imports are rejected at link time with the exact message below. See [`../../specs/wasi/MANIFEST.md`](../../specs/wasi/MANIFEST.md) § “Unsupported by design (not pending)”. |

An import of `wasi:http` is refused with this exact message:

```
wasi:http is not supported by this runtime by design (removed in M17); the pinned HTTP WIT is reference data only. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.
```

This is a deliberate, permanent exclusion — not a pending interface and not a
missing piece of the declared profile. The declared profile is the pinned
WASI 0.3.2 surface **minus** `wasi:http`; the unsupported declarations
(`types`, `handler`, `client`, and the `service`/`middleware` worlds) are
enumerated in `specs/wasi/MANIFEST.md`.

Coverage is enforced at construction: an unimplemented function of a
bound interface is a `host-error`, never a trapping stub.

## Declared limits

| Limit | Effect |
| --- | --- |
| No reactor in the default context | The CLI builds no reactor, so `clocks.wait-until` / `wait-for` signal `host-error` and a would-blocked socket operation returns `would-block` with nothing to wake the parked task later. An embedder that supplies `:reactor` gets both. |
| `@unstable` gating | Annotations are parsed and recorded but not enforced as a feature switch; a gated-off interface (`wasi:clocks/timezone`) simply has no provider, so a world importing it is refused. |
| `proc_raise` (Preview 1) | A valid signal returns `errno::notsup`; an invalid one `errno::inval`. The hosting SBCL process is never signalled. |
| `shutdown` | Not present in the pinned 0.3.2 `sockets` WIT revision, so it is not provided. |

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
name, and a component mixing release lines is a `link-error` before any
guest code runs.

`src/wasi-preview2.lisp` binds 27 of the 31 stable interfaces at their
exact release names. The pinned SQLite component for this line decodes,
validates, instantiates and executes real SQL through
`wasi:cli/run@0.2.12`, in memory and on disk.

Measured behaviour for both ABIs:
[`../conformance/wasi.md`](../conformance/wasi.md).
