# ADR-002: Instruction IR shape and spelling

Date: 2026-10-23 · Status: accepted

## Context

docs/design/PLAN.md §5.3 requires a typed IR shared by validate/runtime/interp/
codegen ("DecodedModule"), with source-map links.

## Decision

- One `wasm-instr` struct: `op` (keyword), `imms` (plist), `body`
  (nested instruction vector), `offset` (absolute byte offset).
- Op keywords are **hyphenated where the mnemonic contains `_`**
  (`:br-if`, `:br-table`, `:call-indirect`, `:memory.init`,
  `:table.grow`, `:i32.trunc_sat_f64_s`); arithmetic stays dotted
  (`:i32.add`) matching the binary-format tables.
- **Block bodies exclude the trailing `:end`; const-exprs include it.**
  (`:if` then-branch in `body`, else-branch in `imms` `(:else-body …)`.)
- Immediates: `:value` for integer consts, `:bits` for float consts,
  `:local/:global/:table/:func/:label/:type/:data/:elem/:memidx/:dst/:src`,
  memarg plists `(:align n :offset n [:memidx n])`, `br-table`
  `(:targets vector :default n)`.
- Full spelling is normative in `docs/CONTRACTS.md`.

## Consequences

+: Single source of truth; encode/decode/validate/interp/codegen all
   dispatch on the same keywords.
+: `offset` gives the source map docs/design/PLAN.md §5.3 requires (diagnostics
   print module/function/offset).
−: Two block-terminator conventions (with/without `:end`) is subtle;
   early agents repeatedly got it wrong in tests. Mitigated by CONTRACTS
   doc + binary test suite pinning both shapes.
−: An earlier draft of CONTRACTS.md used underscored keywords and
   dotted pairs; validate initially implemented that draft and had to be
   aligned to the decoder (the only real contract break in the project).
