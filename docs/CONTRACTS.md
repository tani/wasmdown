# Cross-slice contracts (wave-0 skeleton, rev 1)

These are the binding contracts between implementation slices. Slices may
change file-internal details, never these interfaces.

## Value representation

| Wasm type | Lisp value |
| --- | --- |
| i32 | non-negative fixnum `0 .. 2^32-1` (unsigned interpretation) |
| i64 | non-negative integer `0 .. 2^64-1` (unsigned interpretation) |
| f32 | `single-float` (NaN payload loss acceptable only via `f32-bits`) |
| f64 | `double-float` |
| v128 | one `(simple-array (unsigned-byte 8) (16))` object (loop 24; scalarized lane ops — never per-lane boxes) |
| funcref/externref | runtime func-instance or opaque host object; `nil` = null ref |

Signed views: `wasmdown.numops:i32-s`, `i64-s` (and `_u` identity helpers).

## Module IR (`src/types.lisp`, package `wasmdown.types`)

- `wasm-module` slots: types imports funcs tables mems globals exports
  start elems datas customs data-count (vectors; start/data-count may be nil).
- Instructions: `wasm-instr` with `op` (keyword like `:i32.add`), `imms`
  (plist), `body` (vector of wasm-instr, structured ops only), `offset`
  (absolute byte offset into the original binary — source map).
- Memory instructions: memarg plist `(:align n :offset n)`; when the
  binary set bit 6, also `(:memidx n)`; default memidx 0.
- Blocks: `:block`/`:loop`/`:if` have `imms (:bt blocktype)` where
  blocktype is `nil` (empty), a valtype keyword, or an integer typeidx;
  `body` holds the then-branch vector; `:if` with else stores
  `(:else-body vector)` in imms. Immediates are plists: `:br`/`:br-if`
  use `(:label n)`; `:br-table` uses `(:targets vector :default n)`;
  `:call` uses `(:func n)`; `:call-indirect` uses `(:type n :table n)`;
  `:local.get`/`:local.set`/`:local.tee` use `(:local n)`;
  `:global.get`/`:global.set` use `(:global n)`;
  `:table.get`/`:table.set` use `(:table n)`;
  `:select` uses `(:types vector-or-nil)`;
  `:i32.const`/`:i64.const` use `(:value n)` where n is the SIGNED
  literal from the source (decoder reads s32/s64; interp/codegen wrap
  into the unsigned domain at use);
  `:f32.const`/`:f64.const` use `(:bits n)` (raw bit pattern).
- Structured op keywords are hyphenated: `:br-if`, `:br-table`,
  `:call-indirect`, `:memory.init`, `:data.drop`, `:memory.copy`,
  `:memory.fill`, `:table.init`, `:elem.drop`, `:table.copy`,
  `:table.grow`, `:table.size`, `:table.fill` (0xFC-prefixed family);
  arithmetic stays dotted (`:i32.add`).
- Active elem mode: `(:active tableidx expr)`; passive `(:passive)`;
  declarative `(:declarative)`. Data active: `(:active memidx expr)`.
- func locals: list of `(valtype . count)` pairs (declared order).

## Phase separation (PLAN.md §12.2)

`decode-error` (malformed binary) / `validation-error` (bad typing) /
`link-error` (import mismatch) / `instantiation-error` (segment bounds etc.)
/ `wasm-trap` (runtime) / `wasm-exception` (guest throw) are distinct
condition classes, never interchangeable.

## Runtime objects (`src/runtime.lisp`, package `wasmdown.runtime`)

- `make-module-instance` returns a struct with memories/tables/globals/
  funcs (vectors of instance objects) and exports (alist name → value).
- func-instance kinds: `:wasm` (wasm-funcidx + module-instance backpointer)
  or `:host` (host-fn = Lisp function of (arg ...) → results list).
- Host functions receive Lisp-domain values per the table above and return
  a *list* of results.
- Memory is a `(vector (unsigned-byte 8))` with 64 KiB pages; bounds check
  is `(+ ea w) <= size` computed in fixnum/bignum domain (no host overflow).
- Trap kinds: `:unreachable :out-of-bounds-memory :out-of-bounds-table
  :integer-divide-by-zero :integer-overflow :invalid-conversion
  :null-reference :indirect-call-type-mismatch :call-stack-exhausted`.
- Trap kind `:integer-divide-by-zero` and `:integer-overflow` are separate.

## Test harness

`tests/harness.lisp` provides `test`/`is`/`assert-trap`/`assert-raises`;
tests live in `tests/*.lisp` in package `wasmdown.test`; `make test-unit`
runs everything, nonzero exit on failure.

## Slice interfaces (to implement)

- `wasmdown.binary`: `decode-module (vector (unsigned-byte 8)) → wasm-module`;
  `encode-module (wasm-module) → byte vector` (round-trip test).
  Bounds-checked reader; padded LEB128 accepted; `decode-error` with offset.
- `wasmdown.numops`: exact numeric primitive for every scalar opcode.
- `wasmdown.validate`: `validate-module (wasm-module) → t` or `validation-error`.
- `wasmdown.runtime` + `wasmdown.interp`: instantiate + invoke with start
  function, segments, imports; `wasm-call` trampoline shared with codegen.
- `wasmdown.codegen`: compile each function via SBCL `COMPILE` of emitted
  LET/TAGBODY forms; same `wasm-call` entry; parity tested vs interp.
- `wasmdown` API: `read-module validate-module instantiate invoke …`.
- CLI: `validate`/`run`/`compile`/`inspect` subcommands per PLAN.md §11.2.
