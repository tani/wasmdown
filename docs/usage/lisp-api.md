# Lisp API

The public surface is the package `wasmdown` (`src/packages.lisp`,
implemented in `src/api.lisp`). Subsystems live in their own packages —
`wasmdown.types`, `wasmdown.binary`, `wasmdown.validate`,
`wasmdown.runtime`, `wasmdown.interp`, `wasmdown.codegen`,
`wasmdown.text`, `wasmdown.component`, `wasmdown.wit`,
`wasmdown.wasi` — and are loaded by the single system `:wasmdown`.

```commonlisp
(asdf:load-system :wasmdown)
```

## Entry points

| Function | Signature | Semantics |
| --- | --- | --- |
| `read-module` | `(source)` | Decode `SOURCE` — a pathname, namestring, or `(vector (unsigned-byte 8))` — into a module object. Signals `decode-error` (with byte offset) on malformed input. Does not validate. |
| `module-bytes` | `(module)` | The module as a **canonical re-encoding**, not the bytes it was read from. Decoding the result yields an equivalent module. |
| `validate-module` | `(module)` | Static validation only: no compilation, no host allocation, no execution. Returns the module; signals `validation-error`. |
| `instantiate` | `(module &key imports mode before-start)` | Validate, resolve and type-check imports (`link-error`), allocate, install the `mode` backend, initialise segments (`instantiation-error`), publish exports, call `before-start`, then run the start section (`wasm-trap` propagates). Returns the instance. |
| `invoke` | `(instance export-name &rest args)` | Call an exported function; returns the results list. Traps propagate unchanged. An unknown export is `link-error`; a non-function export or wrong arity is `host-error`. |
| `make-imports` | `()` | A fresh, empty import table. |
| `imports-set-function` | `(table module field lisp-fn param-types result-types)` | Register a Lisp function as import `module.field`, with spec-level link-time type checking. Function imports only. |
| `instance-export` | `(instance name)` | The raw exported object (function, memory, table, or global instance); `link-error` when absent. |
| `module-features` | `(module)` | Extension keywords the module actually uses, in fixed order: `:imports :memory :multi-memory :table :globals :start :multi-value :sign-extension-ops :saturating-float-to-int :bulk-memory :reference-types`. |
| `run-string` | `(hex)` | Build a module from a hex-encoded binary (whitespace ignored). `host-error` on bad hex, `decode-error` on a malformed binary. It takes hex, not WebAssembly text. |

Imports are passed to `instantiate` either as the table built by
`make-imports` or as a raw alist of `((module field) . runtime-object)`.

Condition classes are exported from the same package; see
[`errors.md`](errors.md).

## Adjacent entry points

| Call | Package | Use |
| --- | --- | --- |
| `parse-wat` / `parse-wat-file` | `wasmdown.text` | WebAssembly text to a module object. |
| `component-p` / `decode-component` / `validate-component` | `wasmdown.component` | Component binary front end. |
| `make-wasi-context` / `open-wasi-preopen` | `wasmdown.wasi` | Build explicit capabilities. |
| `run-preview1` | `wasmdown.wasi` | Run a Preview 1 command module (`_start` once); returns the raw u32 status. |
| `run-component` | `wasmdown.wasi` | Run a command component; returns `(values status store)`. |
| `compile-module-functions` | `wasmdown.codegen` | The compiled backend behind `:mode :compiled`. |

[`wasi.md`](wasi.md) covers the capability objects; the CLI is
[`cli.md`](cli.md).

## Value domain

Lisp values crossing the API are exactly the ones in
[`../CONTRACTS.md`](../CONTRACTS.md#value-representation):

| Wasm type | Lisp value |
| --- | --- |
| i32 | non-negative fixnum `0 .. 2^32-1` (unsigned view) |
| i64 | non-negative integer `0 .. 2^64-1` (unsigned view) |
| f32 | `single-float` |
| f64 | `double-float` |
| v128 | one `(simple-array (unsigned-byte 8) (16))` |
| funcref / externref | a runtime function instance or an opaque host object; `nil` is the null reference |

Signed views come from `wasmdown.numops:i32-s` and `i64-s`. Arguments
are passed as individual values and results come back as a list;
`imports-set-function` uses the mirror convention — Lisp arguments in,
multiple values out, coerced to `()` when the import returns nothing.

## Backend selection

`mode` is explicit and applies to the functions **this module defines**:
`:interpret` (default) or `:compiled`.

| Mode | Behaviour | Caveats |
| --- | --- | --- |
| `:interpret` | Every guest call runs the reference interpreter. | Always available; the semantic oracle for the other backend. |
| `:compiled` | Every defined function is compiled before the start section runs, and the compiled entries are installed for every later call (exports, table elements, `ref.func`, imported uses). | Signals `host-error` if the backend module is not loaded, and `codegen-error` when a function uses an instruction the compiler cannot lower. Nothing guest-visible has run at that point: compilation happens before active segments and before the start section, and there is **no interpreter fallback**. |

The order inside `instantiate` is fixed: validation -> import resolution
-> allocation -> backend installation (`:compiled` compiles here) ->
active segments -> exports -> `before-start` -> start section. A compile
refusal therefore precedes any table/memory mutation and the start
section itself runs through the selected backend.

Compilation fills `compiled-entry` on the existing func-instances; it
never replaces them. A reference held by an export, a table, an element
segment or another module's import is the same object before and after,
and an imported Wasm function keeps the backend of the instance that
defined it (`wasmdown.runtime:func-instance-backend`).

Call protocol: both backends go through `wasmdown.runtime:wasm-call` ->
`invoke-wasm`, which owns guest depth (`*guest-depth*`,
`*guest-depth-limit*`, default 1000, trapping `:call-stack-exhausted`),
the tail-transfer trampoline (`return_call*` consumes no depth and no
host stack) and the outermost exception boundary (an uncaught guest
exception traps `:uncaught-exception` there).

The CLI's `WASMDOWN_API_MODE` sets the same choice.

`before-start` is called exactly once with the allocated instance after
active segments and exports exist and before the Wasm start section; it
is the hook the Preview 1 facade uses to bind the guest memory.

## Example

```commonlisp
(let* ((module   (wasmdown:read-module #P"add.wasm"))
       (checked  (wasmdown:validate-module module))
       (instance (wasmdown:instantiate checked :mode :interpret)))
  (wasmdown:invoke instance "add" 42 13))   ; => (55)
```

Imports and a compiled run:

```commonlisp
(let* ((imports (wasmdown:make-imports))
       (module  (wasmdown:read-module #P"math.wasm")))
  (wasmdown:imports-set-function imports "env" "double"
                                 (lambda (x) (* 2 x))
                                 '(:i32) '(:i32))
  (let ((instance (wasmdown:instantiate module
                                        :imports imports
                                        :mode :compiled)))
    (wasmdown:invoke instance "compute" 21)))   ; => (42)
```
