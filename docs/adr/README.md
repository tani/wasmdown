# Architecture decision records

Decisions that shape interfaces other files must not change. The normative
interface text stays in [`../CONTRACTS.md`](../CONTRACTS.md); an ADR records
why it has that shape and what the alternatives cost. The design contract
these decisions serve is [`../design/PLAN.md`](../design/PLAN.md).

| Number | Title | Date | Status | Decision |
| --- | --- | --- | --- | --- |
| [ADR-001](ADR-001-value-representation.md) | Value representation | 2026-10-23 | accepted | Keep i32/i64 as unsigned Lisp integers and f32/f64 as CL floats at every runtime boundary, confining signed and bit-exact views to `numops`. |
| [ADR-002](ADR-002-instruction-ir.md) | Instruction IR shape and spelling | 2026-10-23 | accepted | Use one `wasm-instr` record — `op`, `imms`, `body`, `offset` — with hyphenated keywords for underscore mnemonics and `CONTRACTS.md` as normative spelling. |
| [ADR-003](ADR-003-scope-and-phases.md) | Executable scope, phase separation, guest-recursion guard | 2026-10-23 | accepted · amended | Ship an honest partial engine slice, give every failure phase its own condition class, and trap guest recursion before the host stack dies. The scope item is superseded in part; phase separation and the recursion guard still bind. |
| [ADR-004](ADR-004-codegen-bridge.md) | Interpreter/codegen bridge via the `wasm-call` trampoline | 2026-10-23 | accepted · amended | Route every call through one trampoline, with compiled functions entering as host-shaped closures instead of replacing the interpreter hook. |
| [ADR-005](ADR-005-memory-ceiling.md) | Default memory page ceiling (1024 pages) | 2026-10-23 | accepted | Cap default linear memory at 1024 pages (64 MiB) behind a raisable `*max-pages*` rather than eagerly honoring the 65536-page format limit. |

## Recording a new ADR

- File is `ADR-NNN-short-slug.md` in this directory, numbered in acceptance
  order, headed by `# ADR-NNN: <title>` and one metadata line
  `Date: YYYY-MM-DD · Status: <status>`; the status vocabulary is exactly
  `proposed`, `accepted`, or `superseded by ADR-NNN`, and the body uses
  `Context`, `Decision`, `Consequences`.
- Once accepted a decision is immutable: reversals and narrowings arrive as a
  dated amendment note in the same file, or as a new ADR that supersedes
  it — never as a silent rewrite of the original rationale. An amendment note
  says what changed and where the record lives (normally
  [`../history.md`](../history.md)); it does not accumulate a cycle log of its
  own.
