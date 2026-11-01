# ADR-005: Default memory page ceiling (1024 pages)

Date: 2026-10-23 · Status: accepted

## Context

docs/design/PLAN.md §3.4 permits documented implementation ceilings; §7.4 requires
eager, explicitly checked linear memory. The official suite
(`memory.wast`, `memory_grow.wast`) deliberately instantiates modules
declaring up to `(memory 65536)` (4 GiB) and more as *unlinkable*
tests; a naive eager byte-vector backend tries to honor the full
allocation and exhausts the SBCL heap — an uncatchable, process-killing
failure (observed: a 512 MiB single-allocation death during the first
baseline run; the validator's local-count preflight fixed a second
512 MiB case, this ADR fixes the memory-pages one).

## Decision

`wasmdown.runtime:*max-pages*` defaults to **1024 pages (64 MiB)**,
not the format limit of 65536. `make-memory-instance` rejects min
pages above the ceiling with a `wasm-trap` (`:out-of-bounds-memory`,
"memory too large"); `memory.grow` above the ceiling returns the
specified -1 failure value instead of allocating. The ceiling is an
ordinary dynamic variable embedders can raise for real workloads.

## Consequences

+: Unlinkable/oversized-allocation tests in the suite fail *honestly*
   (classified `fail`/`unsupported`) instead of killing the process.
+: docs/design/PLAN.md §3.4 satisfied: the ceiling is documented here, in
   `specs/lock.toml`, and in the README; exhaustion is reported as a
   resource condition, never as a validation error or a crash.
−: Modules legitimately requiring >64 MiB min pages are rejected by
   default — a documented limit, not a silent omission; raise
   `*max-pages*` (and SBCL's `--dynamic-space-size`) for such
   workloads. The docs/design/PLAN.md §7.4 OS-virtual-memory backend (reserve/
   commit) is the future path that removes this ceiling without
   risking heap exhaustion.
