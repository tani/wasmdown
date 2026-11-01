# Command line

```sh
sbcl --script tools/wasmdown.lisp <command> [options] <file>
```

The launcher loads the `:wasmdown` ASDF system from the repository
containing the script and exits with `wasmdown.cli:main`'s status; there
is no installed binary. Its own usage text, printed by `help`:

```text
usage: wasmdown <command> [options] <file>
commands:
    validate [--profile name] <file.wasm>
    inspect <file.wasm>
    compile <file.wasm> --emit-lisp <out.lisp>
    run <file> [--invoke <name> <int> ...]
    run <component> --wasi [--dir HOST[::GUEST]]
          [--env K=V] [arg ...]
    (the network options --net/--listen/--dns are refused: network
     communication is out of scope by design)
    help
exit codes: 0 ok, 1 guest/compile error, 2 usage error
```

## Commands

| Command | Requires | Effect |
| --- | --- | --- |
| `validate` | exactly one file | Decodes, then statically validates. `--profile` is accepted and ignored (one profile only). |
| `inspect` | exactly one file | Validates, then prints counts, import/export signatures, memory and table limits, start index, and the `module-features` keyword list. |
| `compile` | one file and `--emit-lisp <out.lisp>` | Validates, instantiates, and runs the code generator, writing Lisp source. A module with no defined functions is refused. |
| `run` | one file | Executes a core module or a component; grammar below. |
| `help` | nothing | Usage on stdout, exit 0. No arguments at all also prints usage, but exits 2. |

`validate`, `inspect`, and `compile` write results to stdout, diagnostics
to stderr, and exit 1 on any decode, validation, or wasmdown error.

## `run` grammar

`run` dispatches on the component preamble: a component binary is
driven through the WASI command runner, a core module through
`--invoke` (or `_start` with `--wasi`).

| Option | Meaning |
| --- | --- |
| `--invoke NAME INT…` | Call export `NAME` with the given integers; repeats allowed. Core modules only. Results print space-separated, one line per `--invoke`. |
| `--wasi` | Grant the WASI capability set: process streams, guest arguments, and the flags below. |
| `--dir HOST[::GUEST][:ro]` | Preopen a host directory; `GUEST` defaults to `HOST`. `:ro` grants a read-only root (creation, writing, mutation denied). |
| `--env K=V` | One environment variable. |
| `--net` / `--listen` / `--dns` | **Refused (usage error).** Network communication (TCP, UDP and DNS) is out of scope by design; there is no `wasi:sockets` provider for guests and no network authority is ever built. The canonical message is printed by `help` and on refusal. |
| `--` | Ends option parsing; every later token reaches the guest unchanged, including dash-prefixed ones. |

The network options are recognised only to refuse them with this exact
message:

```
Network communication (TCP, UDP and DNS) is not supported by this design; there is no wasi:sockets provider for guests. See docs/usage/wasi.md and specs/wasi/MANIFEST.md.
```

It is a permanent, deliberate exclusion, not a pending interface; the
declaration lives in `specs/wasi/MANIFEST.md` ("Network communication:
wasi:sockets (excluded)").

Positional tokens after the file become guest arguments **only** with
`--wasi`; `argv[0]` is the file path as given. The parser refuses, as
usage errors: any capability flag or guest argument without `--wasi`,
`--invoke` on a component, `--invoke` together with a `--wasi` core
run (which runs `_start` once), guest arguments for a plain core
module, and any unknown option — this runtime has no `wasi:http`
provider, so there is no transport option either.

A WASI run's preopen handles belong to the invocation; the CLI closes
them after every run, success or not.

## Backend selection

`WASMDOWN_API_MODE` picks the execution backend for all guest code —
core `--invoke`, Preview 1 `_start`, and the cores embedded in a
component:

| Value | Backend |
| --- | --- |
| unset or `interpret` | Reference interpreter (default) |
| `compiled` | SBCL compilation of the module's functions |
| anything else | Usage error (exit 2) |

See [`lisp-api.md`](lisp-api.md) for its caveats.

## Heap for the compiled backend

`sbcl --script tools/wasmdown.lisp` starts with SBCL's default dynamic
space, which is not enough to compile a multi-megabyte guest (the pinned
SQLite fixture needs roughly 8 GiB at compile time). Run a `--wasi`
component under `:compiled` with an explicit heap:

```sh
WASMDOWN_API_MODE=compiled sbcl --dynamic-space-size 8192 \
  --script tools/wasmdown.lisp run app.wasm --wasi -- /work/db
```

Compilation is eager (every defined function before the start section), so
this is a compile-time cost, not an execution-tier switch.

## Exit codes

| Code | Meaning |
| --- | --- |
| `0` | Success, including `help`. A WASI run whose guest exits 0. |
| `1` | Guest or compile failure: decode, validation, link, instantiation, trap, or codegen; also a Preview 1 / component run whose exit status is non-zero. |
| `2` | Usage or host-invocation failure: bad argument, bad integer, unknown command or option, missing file, invalid `WASMDOWN_API_MODE`, `host-error` during `run`. |

A WASI guest's exit status is truncated to its low eight bits, so
`proc_exit(300)` becomes `44`; a component command returning `err` maps
to 1.

## Examples

```sh
sbcl --script tools/wasmdown.lisp validate add.wasm
sbcl --script tools/wasmdown.lisp inspect  add.wasm
sbcl --script tools/wasmdown.lisp run      add.wasm --invoke add 42 13
# 55
```

A Preview 1 command guest reading stdin, with guest arguments after `--`:

```sh
printf 'SELECT 6*7;\n' | sbcl --script tools/wasmdown.lisp run \
  tests/fixtures/wasi/sqlite3-p1.wasm --wasi -- -batch :memory:
# 42
```

A component with a writable directory and an environment variable:

```sh
sbcl --script tools/wasmdown.lisp run app.wasm \
  --wasi --dir .::/work --env HOME=/work -- /work/db.sqlite
```

Capabilities and what is refused: [`wasi.md`](wasi.md). Error classes
and trap kinds: [`errors.md`](errors.md).
