# WASMDOWN: A Common Lisp–Native WebAssembly Runtime

**Technical specification and implementation plan — revision 1.1 (22 October 2026)**

> **Status:** Proposed architecture, **partially implemented and
> conformance-measured** — the core subset, the Component Model (M7–M15)
> and the WASI 0.3.2 host providers (M16) are in the tree; see
> `README.md` and `specs/lock.toml`. `declared_support` = `partial`
> (§2). Baseline: Core WebAssembly 3.0, the Component Model adopted by WASI 0.3.2, and optional backward-compatibility facades for WASI 0.1/0.2. Primary platform: SBCL on Linux x86-64, then Linux AArch64/macOS. Production goal: zero mandatory third-party Common Lisp libraries.

**Changes in revision 1.1:** Converted the complete HTML specification to Markdown; expanded §12 with executable test-harness contracts, official test suite pinning, failure-phase classification, Component Model and WASI test adapters, fuzz/differential testing, CI commands, machine-readable reports, and exact conformance/release gates.

## Contents

- [1. Executive decisions](#1-executive-decisions)
- [2. Standards and version policy](#2-standards-and-version-policy)
- [3. Conformance contract and feature inventory](#3-conformance-contract-and-feature-inventory)
- [4. Subsystems and ownership](#4-subsystems-and-ownership)
- [5. Binary frontend, validation and intermediate representations](#5-binary-frontend-validation-and-intermediate-representations)
- [6. Common Lisp code generation and native compilation](#6-common-lisp-code-generation-and-native-compilation)
- [7. Runtime values, memory and exact arithmetic](#7-runtime-values-memory-and-exact-arithmetic)
- [8. Component Model and WASI 0.3 asynchronous ABI](#8-component-model-and-wasi-0-3-asynchronous-abi)
- [9. WASI providers and operational behavior](#9-wasi-providers-and-operational-behavior)
- [10. Security, isolation and resource management](#10-security-isolation-and-resource-management)
- [11. Public API, command line and repository layout](#11-public-api-command-line-and-repository-layout)
- [12. Verification strategy, official test-suite validation, and release gates](#12-verification-strategy-official-test-suite-validation-and-release-gates)
- [13. Performance engineering and measurement](#13-performance-engineering-and-measurement)
- [14. Milestone-based delivery plan](#14-milestone-based-delivery-plan)
- [15. Risks, decision records and definition of done](#15-risks-decision-records-and-definition-of-done)
- [16. Normative references and verification sources](#16-normative-references-and-verification-sources)

---

## 1. Executive decisions

| Target | Commitment |
| --- | --- |
| **Wasm 3.0** | Normative core semantics |
| **WASI 0.3.2** | Versioned interface snapshot |
| **SBCL first** | Native Lisp machine code |
| **0 mandatory Quicklisp deps** | Runtime dependency policy |

**Architectural thesis.** Parse and validate Wasm as Wasm; optimize in a small, explicitly typed intermediate representation; then emit ordinary Common Lisp functions for SBCL to compile. Keep host resources, Component Model state, and WebAssembly instance state out of the generated code's implicit environment.

```text
                    .wasm module / component
                              |
            [binary decoder + version gates]
                              |
            [core + component validators]---- [WIT parser / type resolver]
                              |
                   [typed, validated IR]
                              |
             [CFG, SSA, safe optimizations]
                              |
                    [Lisp form emitter]
                              |
              [SBCL COMPILE / FASL cache]
                              |
                       [core instance]
                        /           \
          [component canonical ABI]  [WASI 0.1 adapter]
                        |
             [WASI 0.2 / 0.3 providers]
                        |
     [capabilities / async reactor / host OS bindings]
```

> **Non-negotiable distinction:** Core Wasm 3.0 conformance is not equivalent to WASI 0.3.2 conformance. WASI 0.3 requires component binary decoding, linking, Canonical ABI, async lift/lower, streams, futures, resources, and the corresponding versioned WIT interfaces. The current Core 3.0 specification is single-threaded; the separate threads/atomics proposal is a valuable optional compatibility extension, not a prerequisite for claiming Core 3.0 conformance.

**Reference-correctness rule.** For every accepted module `M`, input `I`, and admissible host-provider behavior, optimized execution must agree with the official abstract semantics on all specified observable outcomes (results, imports/exports, stores, traps, and exceptions), allowing only nondeterminism explicitly admitted by the standards:

\[\operatorname{Obs}(\operatorname{Run}_{\mathrm{CL}}(\operatorname{Compile}(M), I))\;\sim_{\mathrm{spec}}\;\operatorname{Obs}(\operatorname{Eval}_{\mathrm{Wasm}}(M,I)).\]

Compliance comes before throughput. Compiled and reference execution paths may differ internally, but must implement identical Wasm semantics.

---

## 2. Standards and version policy

| Document / release | Required implementation contract | Revision handling |
| --- | --- | --- |
| WebAssembly Core 3.0 (versioned 3.0 specification; inspected 22 Oct 2026) | Normative binary structure, validation, instructions, execution, embedding semantics, text grammar, profiles; no omission of standard features. | Pin the exact specification revision and upstream conformance-suite commit; take later errata as reviewed updates. |
| Component Model | Component encoding, validation, linking, WIT types, Canonical ABI, component instances, resources, async built-ins adopted for WASI. | Pin component-model source commit and enabled feature gates in a machine-readable manifest. |
| WASI 0.3.2 (13 Oct 2026) | Stable published WIT surface and versioned package/interface semantics. Adopted Component Model features include async lift/lower, future, stream, map, implements, and external-id. | Vendor exact stable WIT snapshots and SHA-256 checksums; use version-specific dispatch, never floating main-branch WIT. |
| WASI 0.2 and 0.1 | Distinct backward-compatibility targets. WASI 0.1 uses direct core-module imports; 0.2 is component-based with its own `wasi:io` model. | Keep separately tested adapters. Do not confuse 0.2 and 0.3 interface or lifecycle semantics. |
| Threads/atomics proposal | Optional extension: shared memory and atomic operations, only under an explicit extension feature flag. | Version-pin proposal tests; do not silently label it part of normative Core 3.0. |

At the baseline date, WASI 0.3.2 is the latest verified release; the WASI website's roadmap may still display it as “planned,” but the versioned GitHub release dated 13 October 2026 is authoritative. WASI is still pre-1.0; “stable” here refers to published, non-prerelease 0.3 interfaces, not a final WASI 1.0 standard.

Support declarations have three distinct values: `conformant` (every applicable standardized feature and test implemented), `partial` (some missing), and `extension` (non-core proposal). Never advertise “full Wasm 3.0” merely because mainstream C/Rust programs run.

---

## 3. Conformance contract and feature inventory

### 3.1 Core 3.0 — required

| Family | Implement and verify |
| --- | --- |
| Module format | Magic/version, LEB128 decoding that accepts every specification-permitted padded encoding, every core section and index space, imports/exports, custom sections, element/data segments, initialization/start order, error taxonomy. |
| Control flow | Blocks/loops/if, multivalue, branch tables, polymorphic unreachable code, direct/indirect/ref calls, return, tail calls (`return_call*`), structured exceptions and tags (`try_table`, `throw*`). |
| Numbers | All i32/i64/f32/f64 operations, conversions, saturation, bit operations, trap conditions, roundings, payload-sensitive reinterpretation and legal NaN results. |
| Memory/tables | Multiple memories; memory64 and 64-bit table indices; load/store, grow, fill/copy/init, drop, alignment interpretation, bounds checks, shared identity across imports and exports. |
| Vector | Complete v128 fixed-width SIMD and relaxed SIMD semantics; scalar reference path acceptable for performance only, not missing operations. |
| References and GC | Nullable/non-null references, typed function references, recursive/subtypes and equivalence, struct/array/i31, packed fields, heap casting/testing, defaults, reference identity, `any`/`extern` conversions. |
| Validation | Complete typing/subtyping, control stack, definite initialization of non-defaultable locals, constant expressions and all module-level invariants. |
| Profiles | Default Core 3.0 behavior and the specified deterministic profile as a separate opt-in mode, with semantic restrictions enforced when selected. |

**Threads:** not included in the above normative inventory. A later proposal-compliant mode may add atomics, shared memory, wait/notify and a defined host threading model. It must not alter single-threaded module behavior.

### 3.2 Component/WASI — required

Decode and validate components (not only core modules); recursively instantiate embedded modules/components; implement component instance, alias, type and import/export linkage; support WIT type shapes, borrowed/owned resource semantics, canonical lift/lower and post-return cleanup; support WASI 0.3 async functions, futures and streams including backpressure, cancellation and lifecycle; implement version-pinned stable WASI interface providers. Legacy WASI 0.1 and 0.2 remain separately tested compatibility profiles.

### 3.3 Deliberate exclusions

Browser DOM, the JavaScript/Web embedding APIs, dynamically loading arbitrary native libraries from guest code, unratified WASI proposals, and unstandardized Wasm proposals are *not* implied by Core/WASI conformance. A source-level C compiler or Rust toolchain is not part of the runtime deliverable. The `.wat`/`.wast` parser is nevertheless included as a development and conformance tool because the core specification defines a text format.

### 3.4 Allowed implementation limits

Expose documented ceilings for module/code sizes, memory allocation, instance count, call-depth, compilation time, and host resources. These are implementation/resource-limit failures, never silent feature omissions. For a valid program, allocation exhaustion must not be reported as a validation error. Memory growth failure must use its specified return behavior.

---

## 4. Subsystems and ownership

| System | Ownership | Must not know |
| --- | --- | --- |
| `wasmdown.binary` | Bounded binary decoding, section parsing, component/core discrimination, offsets and diagnostics. | SBCL code generation or OS capabilities. |
| `wasmdown.types` | Core recursive type graphs, canonicalization, subtyping and comparison. | Host I/O. |
| `wasmdown.validate` | All static semantic invariants and verified stack types. | Operating system. |
| `wasmdown.ir` | Typed Wasm IR, CFG edges, SSA/phi, exception regions, source locations. | Unvalidated syntax or generated host symbol names. |
| `wasmdown.interp` | Simple reference evaluator for every opcode, GC instruction and trap. | SBCL optimizer assumptions. |
| `wasmdown.codegen` | Verified IR to whitelisted Common Lisp forms; optimization, function compilation and FASL cache. | Raw filesystem/network authority. |
| `wasmdown.runtime` | Module instances, functions, globals, tables, memories, tags, heap refs, host calls. | WASI interface naming. |
| `wasmdown.component` | Component types, linking, resources, canonical lift/lower, async state machines. | Concrete Linux syscalls. |
| `wasmdown.wasi` | WASI 0.3.2 providers, separate 0.2/0.1 providers and world entry points. | Compiler or IR rewriting. |
| `wasmdown.host` | Capabilities, filesystem handles, sockets, clocks, RNG, TLS adapters, reactor. | Guest-invented ambient privileges. |

All cross-subsystem interfaces are explicit. A core module never acquires a host capability by symbol lookup, dynamic variable access, FFI or parsing a guest-supplied name. Module instantiation injects a finite import table with checked types and capabilities.

---

## 5. Binary frontend, validation and intermediate representations

### 5.1 Binary decoding

Implement a bounds-checked streaming reader over byte arrays and files. Track the exact absolute byte offset, enclosing section, declared vector lengths and nested decoding context. Reject out-of-range or excessively long encodings, but accept spec-permitted non-minimal LEB128 encodings (e.g. a padded unsigned integer within its maximum byte length); check integer overflow *before* allocating vectors or advancing counters. Keep custom sections as inert bytes (with optional verified decoders). Never call Lisp `READ` on untrusted Wasm/WIT input.

### 5.2 Validation

Implement the specification's operand/control-stack algorithm, including stack polymorphism after unreachable instructions. Separate decoding success, validation success, import matching, instantiation success, start-function outcome, and runtime traps. For recursive GC types, implement iso-recursive type equivalence/subtyping with bounded graph traversal and memoization. Retain proof-by-construction facts in the IR: typed uses, control targets, verified memory/table indices, mutable field restrictions, and non-defaultable-local initialization.

### 5.3 IR stages

```text
DecodedModule
  -> ValidatedModule      ; immutable, all static invariants discharged
  -> StackIR              ; each opcode has typed operands/results
  -> CFG                  ; explicit block params, branch targets, exceptions
  -> SSA                  ; local/stack values as virtual registers
  -> OptimizedSSA         ; semantics-preserving transformations
  -> LispForms            ; preapproved CL AST only
```

Do not translate raw stack opcodes directly to nested Lisp expressions as the primary optimization design: that makes branch joins, multi-values, traps, GC types, and async suspension harder to preserve. Maintain source-map links from each instruction to the original binary offset and function number.

### 5.4 Core IR invariants

- Every SSA value has an exact Wasm type (or a validated, typed reference subtype); every CFG edge passes the correct block parameter/result tuple.
- Instruction order preserves effects: memories, tables, globals, exceptions, host calls, traps, allocation and canonical-resource actions are not freely reorderable.
- Exception handlers and tail-call sites are represented explicitly; generic Lisp exceptions are not interpreted as guest Wasm exceptions.
- Optimization works only on *validated* IR. A transformation carries legality predicates and differential tests.
- IR serialization is versioned and untrusted caches are never evaluated as Lisp forms.

### 5.5 Baseline optimizer

Implement typed constant folding with width-specific overflow semantics; dead pure-code elimination; simple block flattening; local/stack copy propagation; common-subexpression elimination only within proven effect-safe regions; sparse conditional constant propagation; phi elimination via correct parallel copies; and loop-invariant bounds-check hoisting only with sound proof of range and memory-growth effects. The first backend should favor predictable code size over aggressive inlining.

---

## 6. Common Lisp code generation and native compilation

### 6.1 Compilation contract

The production backend emits ordinary Common Lisp ASTs and compiles per Wasm function with SBCL `COMPILE`, optionally producing readable source and local FASLs for ahead-of-time execution. It does not generate native machine code directly, nor require LLVM/GCCJIT/Cranelift at runtime.

```commonlisp
;; Illustrative generated shape: not a complete or implemented API.
(defun %m17-f2 (instance x)
  (declare (optimize (speed 3) (safety 1))
           (type (unsigned-byte 32) x))
  (let ((v0 x) (v1 0))
    (declare (type (unsigned-byte 32) v0 v1))
    (tagbody
     entry
       (setf v1 (%wasm-i32-add v0 1))
       (go exit)
     exit)
    v1))
```

Emit code into generated, isolated packages with private, compiler-controlled symbols. Guest bytes/identifiers become inert string metadata, never executable Lisp identifiers or reader forms. Compilation diagnostics must contain module/function/offset data. The compiler may generate a call to a runtime primitive only from a finite, audited opcode-to-primitive mapping.

### 6.2 Calls, blocks, exceptions and tail calls

Lower intra-function CFG to `LET`/`TAGBODY`/`GO` and typed temporaries. Implement parameterized block joins with parallel moves; multiple returns through documented internal conventions (unboxed fast paths where verified, `VALUES` for modest fixed arities, tuple fallback at boundaries). Direct/indirect/reference calls dispatch through typed function instances and preserve import provenance. Represent guest exceptions with dedicated tags/records and handler regions, distinct from host Lisp conditions.

**Tail calls:** SBCL is not guaranteed to optimize every cross-function Lisp tail call. Use a tail-call trampoline or equivalent explicit activation protocol to guarantee bounded host-stack usage for `return_call`, `return_call_indirect` and `return_call_ref`. Optimize self-tail calls into CFG backedges; optimize same-signature groups only when semantics remain unchanged.

### 6.3 Compile modes

| Mode | Purpose | Guarantees |
| --- | --- | --- |
| `:interpret` | Reference path / debugging / compiler oracle. | Full semantic coverage; no performance promise. |
| `:baseline` | Fast startup and per-function native compilation. | All Core 3.0 features; conservative transformations. |
| `:optimized` | SSA optimization, specializations, bounds-check hoisting and optional SIMD intrinsics. | Same observables and error categories as baseline. |
| `:aot` | Validated IR to `.lisp`/`.fasl`, cacheable locally. | ABI/version/CPU/OS/cache key matches exactly. |

### 6.4 FASL cache

Key = SHA-256 of input bytes, spec profile, compiler/runtime versions, optimizer configuration, target CPU features, SBCL version and architecture, and relevant host ABI. Treat cached native artifacts as trusted executables: load only from a private user-owned directory with integrity verification. Atomic writes; deterministic cache eviction; no acceptance of arbitrary external FASLs.

---

## 7. Runtime values, memory and exact arithmetic

### 7.1 Integer representation

Wasm integer operations are modulo \(2^{32}\) or \(2^{64}\), with distinct signed and unsigned interpretations. Use specialized internal helpers to impose the appropriate word width at each value-producing operation:

\[\operatorname{word}_{n}(x)=x\bmod 2^n,\qquad \operatorname{signed}_{n}(x)=\begin{cases}x,&x<2^{n-1},\\x-2^n,&x\geq 2^{n-1}.\end{cases}\]

On 64-bit SBCL, i32 values generally fit fixnums and can use typed modular arithmetic. i64 must be designed deliberately: Common Lisp fixnums cannot represent all 64-bit bit patterns. Favor SBCL-supported machine-word unboxing and signed-64-bit bit representations, with unsigned-order helpers; do not rely on generic bignum arithmetic in inner loops. Maintain a fully portable integer fallback and differential-test every signed/unsigned edge case.

### 7.2 Floating point

Implement precise f32/f64 conversion/truncation and saturation traps; infinities, negative zero, nearest-even and NaN rules. Normalize SBCL floating-point trap settings at guest entry/exit without leaking them into unrelated host computation. Fast paths use native floating-point instructions only when the resulting behavior is permitted by Wasm. Preserve bit patterns for `reinterpret`, loads, stores and sign/payload-sensitive operations; never replace them with ordinary numeric equality or serialization. Use explicit helpers for tricky min/max, rounding and f32 narrowing. Relaxed SIMD is allowed only the spec-sanctioned result sets, not arbitrary host-dependent behavior.

### 7.3 v128

Provide a portable reference representation (two 64-bit words or a 16-byte vector) and completely scalarized reference SIMD semantics first. For optimized SBCL targets, use feature-detected intrinsic or carefully isolated architecture-specific implementations where available; avoid making optional SIMD libraries production dependencies. Never allocate per-lane boxed objects in a hot loop when the backend can keep values in virtual registers.

### 7.4 Linear memory

Use an explicit `memory-instance` object with address width, page count, maximum, backing region, generation, and access capabilities. Start with a byte-vector-backed contiguous memory for manageable memory32 workloads. Add an SBCL/OS-virtual-memory backend (reserve/commit/grow) for large memory32 and memory64 while preserving identical semantics. Both backends support multi-memory, imported memory identity, and zero initialization.

For an access of \(w\) bytes, the range check must be performed without host integer overflow. In the mathematical domain, an access is allowed exactly when its specified effective address \(e\) obeys \(0\leq e\) and \(e+w\leq S\), where \(S\) is the current byte length. Implement the spec's address calculation for the selected 32-/64-bit memory and check the complete interval *before* unguarded native access. `memory.copy` must handle overlap like `memmove`. Growth failure must preserve the original memory and report the specified failure value.

> **Safety:** Do not use naked SAP (system-area-pointer) access with an assumed fault handler as the sole Wasm bounds check. SBCL/OS signal behavior is not a portable substitute for the Wasm trap semantics. Start with explicit checks; hoist/eliminate them only when justified by range analysis and invalidated appropriately on growth.

### 7.5 Tables, references, GC and traps

Represent tables as mutable, typed vectors of reference handles; preserve nullability and validated type relations for indirect/ref calls and mutations. Represent Wasm-GC structs and arrays as Lisp-managed objects with stable opaque handles, explicit Wasm heap-type IDs, packed-field normalization and identity checks. Use Lisp GC for backing storage, but explicitly implement Wasm subtype, field mutability and cast rules; Lisp's CLOS subtype system is not a substitute. Preserve imported `externref` identities and rooting.

Trap categories include unreachable, out-of-bounds memory/table, division by zero, integer overflow, invalid conversions, null references, cast failures where specified, and indirect-call type errors. Guest traps are non-catchable by Wasm; guest exceptions are catchable only as specified. Host implementation/resource errors remain distinguishable from both.

---

## 8. Component Model and WASI 0.3 asynchronous ABI

### 8.1 Component binary and type system

Implement layer discrimination; component sections, nested core modules and components, instantiation, aliases, exports and imports, WIT-defined interface/world matching, resource and method types, versioned names and component-level type matching. The WIT frontend must cover all component language constructs adopted by WASI 0.3.2, including `map<K,V>` and the adopted `implements`/`external-id` annotations.

### 8.2 Canonical ABI

Implement canonical `lift`/`lower`, canonical memory/realloc/post-return configuration, flattening thresholds and indirect parameter/results, string encodings, lists, records, tuples, flags, variants, options, results, enums, maps and resource handles according to the pinned ABI. Honor ownership of borrowed/owned handles, method/destructor invocation, lifetime tables and component-instance isolation. Reject incorrect layouts, provenance and stale handle use with the correct component/host error semantics.

### 8.3 Async runtime

WASI 0.3 makes `async func`, `stream<T>` and `future<T>` Canonical ABI constructs; it removes `wasi:io`. The component scheduler therefore owns task state, wake propagation across arbitrarily many nested components, bounded buffering, backpressure, cancellation, completion, error propagation and cleanup. Follow the precise pinned canonical ABI transition/lifecycle rules; do not model streams as freely duplicable Lisp sequences.

```text
component task  -> READY -> RUNNING -> SUSPENDED -> READY -> DONE
                                 \                        \
                                  -> CANCELLED              -> ERROR

Yield-capable guest function:
   compiled state machine {resume-pc, typed live values, handles, exception state}
   -- awaits host or component future without retaining an unbounded CL call stack
   -- resumes only after scheduler signals specified readiness
```

Implement suspension by compiler-generated continuation/state-machine frames at async yield points; use a cooperative reactor for single-threaded Core 3.0. Crucially, suspension must propagate through the entire guest call chain: mark potentially suspending imports/canonical calls in typed IR, propagate the `may-suspend` effect through call-graph strongly connected components, and treat indirect calls conservatively. Transform every affected caller into a resumable function (or use an explicit heap-backed activation stack), preserving live SSA values, pending returns and guest exception regions. Synchronous leaf functions can retain direct compiled Lisp calls. Ordinary CL stack frames cannot be assumed resumable. This is separate from, and does not require, the WebAssembly threads proposal. A host poller (`select`/poll, and optionally epoll/kqueue) drives file/socket readiness. Conformance must exercise nested component composition, not merely a component calling the host directly.

### 8.4 Compatibility architecture

WASI 0.2 keeps its own canonical interfaces and `wasi:io` resource semantics, mapped to the same internal host capability abstractions where possible. WASI 0.1 uses the legacy core-module syscall import namespace and its own pointer/memory ABI. Explicitly keep three external facades; do not mix public interface version semantics just to reuse private implementations.

---

## 9. WASI providers and operational behavior

| Package / world | Responsibilities and acceptance cases |
| --- | --- |
| `wasi:cli` | Arguments, environment, working-directory exposure when provided, stdin/stdout/stderr streams, exit handling, command world and correct async `run` entry. |
| `wasi:clocks` | Wall and monotonic clocks; precision and subscription/sleep behavior per exact WIT; monotonicity and host-clock failure semantics. |
| `wasi:random` | Secure random bytes via OS entropy, well-specified insecure variants, host resource/length handling; never substitute `CL:RANDOM` for cryptographic randomness. |
| `wasi:filesystem` | Preopened descriptor authorities, relative paths, file and directory operations, metadata, symlink restrictions, stream reads/writes, rights, deletion/rename, resource closing. |
| `wasi:sockets` | IP lookup, TCP/UDP creation and connection/binding/listening, address-family and errors, async stream and network capability restrictions. |
| `wasi:http` | **REMOVED at user direction in M17** (M16 slice 5). The outgoing client and incoming service/middleware worlds, header/body/trailer semantics, backpressure and status/errors are no longer a deliverable; the implementation is recoverable from git history and the vendored `specs/wasi/wasi-wit-0.3.2/http/*.wit` remain as reference data only. |
| Other versioned stable interfaces | Generate required-interface inventory from vendored WASI 0.3.2 release WIT and implement every interface exposed by the selected stable worlds; experimental proposals are feature-gated. |

> **Implementation status (M16, 2026-10-28): implemented, slice-scoped.**
> The pinned WIT is vendored at `specs/wasi/wasi-wit-0.3.2/` (v0.3.2
> release asset, 15 files / 2,448 lines, SHA-256 pinned in
> `specs/lock.toml` + `specs/wasi/MANIFEST.md`) and read by
> `src/wit.lisp`; `src/wasi.lisp` supplies the provider framework and
> the `clocks`, `random`, `cli` (environment/exit/stdio/terminals/`run`),
> `filesystem` (the `*at` capability model + streams + `read-directory`),
> `sockets` (the `wasi-network` authority + tcp/udp + `ip-name-lookup`)
> providers, all under an explicit `wasi-context`. M16 slice 5
> (`wasi:http`, the `types` resources + `client.send` over HTTP/1.1 with
> a pluggable transport) was **REMOVED at user direction in M17**; the
> implementation is recoverable from git history and the vendored
> `specs/wasi/wasi-wit-0.3.2/http/*.wit` remain reference data only.
> Measured: units 481/0, core 21,187/0/73, the component lane
> 1,631/0/0/0 (the lane metric; the probe metric retains the pre-existing,
> machine-load-dependent async flake class). **Residuals:** no
> socket/timer reactor (`wait-until`/`wait-for` unsupported); the nested
> `option<stream<u8>>` host lower; descriptor-dtor/lend accumulation;
> `%abi-lower`'s top-level error-result discriminant; pre-existing
> text/validator limits; unenforced `@unstable` gating; and the
> WASI 0.1/0.2 facades (§8.4). The former HTTP residuals (outgoing
> streaming uploads, incoming service/middleware worlds,
> keep-alive/pipelining/trailers) are **out of scope, not pending**.
> See `README.md`'s *WASI 0.3.2 host providers (M16)* and ADR-003.
> `declared_support` stays `partial`.

### 9.1 Capabilities

Default capabilities are empty except explicitly configured CLI streams/arguments. File roots are established by open host directory handles, not an unchecked string-prefix filter. Where the host supports it, use handle-relative operations (e.g. openat-style primitives) and secure symlink policies that prevent traversal and TOCTOU escape. Allow separately declared outbound network endpoints, listen addresses and DNS policy. A host import must receive an explicitly allocated authority object, not ambient Lisp globals.

### 9.2 OS integration and the minimal-dependency boundary

**Core/compiler:** ANSI Common Lisp plus selected SBCL facilities; no mandatory ASDF ecosystem libraries. **Host layer:** SBCL's bundled facilities and thin, audited, optional OS ABI bindings (e.g. libc/system calls through `SB-ALIEN`). **TLS:** WASI HTTP (`wasi:http`) was REMOVED at user direction in M17, so the runtime ships no HTTP/TLS adapter; an embedder-supplied transport remains an out-of-tree service, and no custom cryptographic TLS is written merely to satisfy a “pure Lisp” slogan.

Important feasibility condition: zero external Lisp packages is realistic; zero operating-system or cryptographic libraries for a general-purpose secure socket/filesystem implementation is not. A no-TLS installation may be conformant as a core Wasm engine; WASI HTTP is out of scope after its M17 removal (recoverable from git history; the vendored WIT stays reference data only).

---

## 10. Security, isolation and resource management

### 10.1 Trusted computing boundary

Untrusted input spans binaries, WIT text, custom sections, guest imports and dynamically computed guest arguments. The decoder/validator and code emitter constitute the critical trusted computing base. Protect against malicious LEB lengths, deep recursion, exponential type comparisons, huge generated functions, symbol interning/resource exhaustion, host-path races, invalid pointer accesses and accidental Lisp condition escape.

Generated machine code shares the SBCL process with the host: Wasm-level memory safety does *not* give hardened native-process isolation. For hostile multi-tenant execution, use separate worker processes with OS-enforced CPU/memory/FS/network limits. WASI capability policy is necessary but not a replacement for process isolation.

### 10.2 Quotas

Implement quotas for decoded bytes/sections, type complexity, validation operations, compiler time, maximum code bytes, memory pages and committed memory, table/GC allocations, open handles, active async tasks, buffered stream bytes, network connections and execution fuel/epoch deadlines. Support cooperative cancellation of guest execution and host I/O. All resource acquisitions have deterministic cleanup on success, trap, host error and cancellation.

### 10.3 Dependency policy

- Production runtime has zero mandatory Quicklisp/third-party Lisp dependencies.
- ASDF source systems are used for packaging; a pinned SBCL release is the primary compiler.
- OS-native functionality may call platform/system libraries through small opt-in adapters; record exact libraries and supported versions.
- WABT, wasm-tools, Wasmtime, reference interpreter, Rust/WIT generators and Python test runners are *development/CI-only* dependencies.
- Test fixtures and standards snapshots are vendored in the development tree with commit hashes and licensing metadata.

---

## 11. Public API, command line and repository layout

### 11.1 Intended Lisp API

```commonlisp
;; Proposed user-facing interface, not implemented code.
(let* ((module (wasmdown:read-module #P"example.wasm"))
       (checked (wasmdown:validate module :profile :wasm-3.0))
       (compiled (wasmdown:compile-module checked :mode :optimized))
       (imports (wasmdown:make-core-imports))
       (instance (wasmdown:instantiate compiled :imports imports)))
  (wasmdown:invoke instance "example_export" 42))

;; WASI 0.3 belongs to the Component Model, not direct core imports.
;; Components require component linking and a WASI 0.3 host provider:
(wasmdown:run-component #P"service.wasm"
                       :wasi-version "0.3.2"
                       :host-config config)

;; Compiler artifacts:
(wasmdown:emit-lisp checked #P"example.generated.lisp")
(wasmdown:emit-fasl checked #P"example.generated.fasl")
```

In the final API, `read-module`, `read-component` and auto-detection may be distinct constructors. A public `validate` method must not implicitly compile, allocate host resources or run start functions. Expose structured condition classes: `decode-error`, `validation-error`, `link-error`, `instantiation-error`, `wasm-trap`, `wasm-exception` (internal guest dispatch), `resource-exhausted`, and `host-error`.

### 11.2 Intended CLI

```sh
wasmdown validate --profile wasm-3.0 program.wasm
wasmdown run --wasi 0.3.2 --mapdir guest=/srv/guest component.wasm
wasmdown run --wasi 0.1 --mapdir guest=/srv/guest legacy-module.wasm
wasmdown compile --mode optimized --emit-lisp out.lisp --emit-fasl out.fasl program.wasm
wasmdown inspect --types --imports --exports --features program.wasm
wasmdown test --spec-suite ...
wasmdown bench --compare wasmtime ./benchmarks/
```

CLI must reject mismatched artifact/WASI ABI combinations (e.g. trying to attach WASI 0.3 component imports directly to a legacy core module), distinguish required capabilities from requested capabilities, print standardized structured diagnostics, and exit with a stable host exit status independent of Wasm trap representation.

### 11.3 Suggested repository

```text
wasmdown/
  wasmdown.asd                     ; ASDF system definitions
  src/
    binary/                       ; core + component decoders
    types/ validation/            ; type graphs + typing rules
    ir/ interp/ optimize/         ; semantics and CFG/SSA passes
    backend/sbcl/ backend/ansi/   ; compiler backends
    runtime/                      ; memory, tables, refs, traps
    component/ wit/ canonical/    ; component model, WIT, async
    wasi/p1/ wasi/p2/ wasi/p3/    ; distinct WASI surfaces
    host/linux/ host/macos/       ; OS and TLS adapters
    api/ cli/
  specs/                          ; pinned source versions & WIT snapshots
  tests/spec-core/ tests/component/ tests/wasi/ tests/fuzz/
  benchmarks/ fixtures/          ; reference results and profiles
  docs/adr/ docs/semantics/       ; architectural decision records
  tools/                           ; WAST runners & build scripts
  ci/                              ; differential and performance matrices
```

---

## 12. Verification strategy, official test-suite validation, and release gates

**Purpose.** Provide reproducible evidence for the entire declared feature surface, not merely a count of programs that happen to execute. The following are **planned tests and acceptance criteria**; no runtime conformance tests have been executed for this proposal. The official suites are necessary but **not sufficient** for a full-compliance claim: a release additionally requires a normative feature inventory, self-authored gap tests, threat-model review, and evidence that every applicable feature is reachable.

### 12.1 Authoritative suites, coverage, and version pins

| Track | Source | Input and expected coverage | Required result |
| --- | --- | --- | --- |
| Core conformance | [`WebAssembly/spec`, `test/core`](https://github.com/WebAssembly/spec/tree/main/test/core) | Original `.wast` specification scripts: decoding, validation, invocation, traps, runtime state, numeric behavior | All applicable cases accepted at the pinned Core 3.0 revision in every advertised execution mode |
| Core cross-proposal aggregation | [`WebAssembly/testsuite`](https://github.com/WebAssembly/testsuite) | Mirror containing core tests and proposal test directories; use for discovery and expressly optional proposal suites | No duplicate counting of core cases; proposals enabled and reported separately |
| Component conformance | [`WebAssembly/component-model`, `test`](https://github.com/WebAssembly/component-model/tree/main/test) | Evolving `.wast` component tests, encoding, validation, instantiation and Canonical ABI | All applicable tests for the features adopted by the pinned WASI release; add missing end-to-end semantic tests |
| WIT/component tooling | [`wasm-tools`](https://github.com/bytecodealliance/wasm-tools) and versioned WIT corpus | Valid/invalid WIT syntax, resolve, type checking, component decode/validate and encode/decode round trips | Compatibility and differential agreement for the pinned feature set; this is an external development oracle, not a production dependency |
| WASI Preview 1 / 0.1 | [`WebAssembly/wasi-testsuite`](https://github.com/WebAssembly/wasi-testsuite) | Prepared guest programs, filesystem, CLI, stdio, exit status and OS behavior | Pass every applicable P1 case on supported hosts |
| WASI Preview 3 / 0.3 | Same `wasi-testsuite`, using its [`prod/testsuite-base`](https://github.com/WebAssembly/wasi-testsuite/tree/prod/testsuite-base) branch | Precompiled WASI 0.3 programs and operation-based test specifications; includes async-related coverage | Pass all version-compatible cases; implement untested stable interfaces with internal integration tests |
| WASI 0.2 compatibility | Pinned WIT from the WASI 0.2 release and WASMDOWN integration corpus | Component imports/exports, `wasi:io` legacy resource semantics and filesystem compatibility (WASI HTTP was removed in M17, so no HTTP compatibility claim is made) | Independent, explicit 0.2 support claim and green matrix; do not infer 0.2 compliance from 0.3 tests |
| Extensions | Individual proposal source repositories | Threads/atomics and other explicitly enabled, non-Core-3.0 proposals | Separate opt-in conformance report; never required for the unextended Core 3.0 claim |

Use the primary [`WebAssembly/spec`](https://github.com/WebAssembly/spec) revision as the source of truth for core tests. The convenient aggregated [`WebAssembly/testsuite`](https://github.com/WebAssembly/testsuite) may also contain proposal cases outside the target; those cases must be identified by provenance and never silently conflated with core requirements. The Component Model repository currently describes its WAST suite as growing; there is **no assumption that it constitutes a complete normative reference implementation**. Supplement it with end-to-end component fixtures and manual feature coverage.

At M0 create `specs/lock.toml` (or equivalent) recording: full git commit IDs for each suite and specification, WASI release/tag and vendored WIT-tree checksum, `wasm-tools`/WABT/Wasmtime versions, SBCL version, compiler flags and target architecture. Never use floating branches in release CI. A separate scheduled job tests newer upstream commits and proposes deliberate lockfile updates.

### 12.2 Core `.wast` command semantics and failure-phase oracle

The test driver is a **stateful spec-script runner**, not a launcher that executes each `(module ...)` form in isolation. It must support modules, named module instances, `(register ...)`, `spectest` imports, `invoke`, `get`, ordered commands and all pinned specification assertions. For each assertion, check *which stage fails*, not simply whether an error appears.

| Assertion / command | Required validation |
| --- | --- |
| `module` / binary module | Decode, validate, link, instantiate and run start function; update script state only as specified |
| `register`, `invoke`, `get` | Resolve correct registry, imports, exports, values, shared state and instance identity |
| `assert_malformed` | Reject as malformed text or binary **at the decoding/parsing stage**, before validation |
| `assert_invalid` | Parse/decode successfully but reject under static Wasm typing/module validation |
| `assert_unlinkable` | Statically valid module must fail import matching or linking |
| `assert_trap` (module instantiation) | Link succeeds but initialization/start traps at the specified execution phase |
| `assert_trap` (action) | The action traps for a corresponding Wasm reason; never convert host errors into a successful trap |
| `assert_exhaustion` | Exercise permitted resource-exhaustion behavior while preventing host-process instability |
| `assert_return` | Compare complete result tuples, scalar widths and ref identities/types, including allowed NaN-result classes |
| Relaxed SIMD assertions | Accept precisely the specification-sanctioned sets of outcomes, including target-dependent alternatives; do not compare to one arbitrarily chosen golden bit pattern |

Test both **textual** and embedded **binary** `.wast` cases, including `quote`/`binary` syntaxes and intentionally malformed modules. WABT's [`wast2json`](https://github.com/WebAssembly/wabt/blob/main/docs/wast2json.md) is permitted as a **test-only** converter for suites it can correctly represent, but it must not determine the semantics of missing or newly added 3.0 features. Keep a native script parser as the final goal; alternatively use a pinned capable `wasm-tools` adapter with an explicit unsupported-command report. A conversion error or unsupported instruction is **not** a passing test.

The test reporter records `decode`, `validate`, `link`, `instantiate`, `execute`, or `host-provider` as the actual failure phase. Host process crashes, timeouts, a Lisp unhandled condition, and a Wasm trap are four distinct outcomes. Expected assertion message text should be matched only when the standard or test requires it; diagnostic prose may differ while error category remains exact.

### 12.3 Compiler-backend parity matrix

Every executable core fixture and in-house generated program is evaluated, where meaningful, across:

1. `:interpret`: straightforward typed reference evaluator with independently implemented primitive semantics where practical.
2. `:baseline`: validated IR compiled through the conservative Common Lisp backend.
3. `:optimized`: SSA/CFG optimizations, specialized intrinsics and selected bounds-check elimination.
4. `:aot`: locally generated/read-back FASL artifacts, checked against fresh compilation (on compatible SBCL/CPU/ABI only).
5. An independent oracle: official spec interpreter where supported, and pinned Wasmtime for newer proposal or integration cases.

Compare observable return values, all written memories/tables/globals, host-call sequence and arguments, exported state, traps, guest exceptions, exit codes, stdout/stderr, effects of imports, and lifecycle state. Normalize **only permitted** nondeterminism (legal NaN variants, relaxed-SIMD alternatives, clock values, scheduling races, OS-generated error values in allowed classes). Never normalize away ordering mistakes, side effects, or leaked capabilities.

For deterministic tests, the central invariant is:

\[
\operatorname{Obs}(\mathrm{interpret}(M,I))
\equiv_{\mathrm{spec}}
\operatorname{Obs}(\mathrm{baseline}(M,I))
\equiv_{\mathrm{spec}}
\operatorname{Obs}(\mathrm{optimized}(M,I)).
\]

Every optimizer pass must also be tested independently: save the pre-pass IR, apply exactly one transformation, and compare behavior before/after. A mismatch gets a minimized `.wast`/`.wasm` reproducer, original random seed, instruction/offset trace and optimizer-pass index. Keep all minimized failures permanently in `tests/regressions/`.

### 12.4 Component Model and Canonical ABI matrix

Test component **decoding**, **validation**, **linking**, **instantiation**, **lifecycle**, and **guest/host calls** as separate phases. Verify all adopted WIT and Canonical ABI constructs in both lifting and lowering directions:

| Feature family | Essential positive / negative scenarios |
| --- | --- |
| Scalar / string | Signed/unsigned widths; UTF encodings; invalid and maximal encodable Unicode; invalid offsets; zero-sized and misaligned cases |
| Aggregate values | Lists, tuples, records, flags, enums, variants, options, results and maps, including nested and zero-length forms |
| Flat vs memory ABI | Register-parameter/result flattening thresholds, indirect aggregates, reallocation failures, traps and post-return sequencing |
| Typed imports/exports | Recursive nesting, aliases, version resolution, subtyping/matching, duplicate/incompatible bindings and missing imports |
| Resources | Owned/borrowed handles, aliases, borrowing scope, destructor invocation, drops, stale handles, cross-instance ownership and leak checks |
| Component composition | A → B → C → host nested calls; reentrant callbacks; mix of imported core instances and component instances |
| Async Canonical ABI | Async lift/lower, task handles, `future<T>`, `stream<T>`, backpressure, suspension/resumption, cancellation and failure cleanup |
| Adopted feature gates | `map<K,V>`, `implements`, `external-id` and any additional features actually present in the pinned release; reject gated features outside the selected profile |

Use differential fixtures against a compatible Wasmtime version, but do not trust matching outputs alone: assert handle-table invariants, exactly-once cleanup, absence of illegal aliasing, and legal pending/final state transitions. Assert that cancellation during pending host I/O releases resources and that suspension is preserved across an arbitrary number of component boundaries.

### 12.5 WASI test runner integration and compatibility suite

The upstream `wasi-testsuite` documents a Python test runner, adapter mechanism and a branch containing precompiled binaries. In CI (development dependencies permitted):

```sh
# Reference workflow documented by upstream:
git clone --branch prod/testsuite-base https://github.com/WebAssembly/wasi-testsuite
cd wasi-testsuite
python3 -m pip install -r test-runner/requirements.txt
./run-tests --runtime adapters/wasmtime.py

# WASMDOWN target only AFTER implementing and registering the adapter:
./run-tests --runtime adapters/wasmdown.py
```

`adapters/wasmdown.py` is a **proposed new adapter**, not an existing upstream file. It should invoke the WASMDOWN CLI, pass preopened directory mappings and scoped environment variables, translate standard proposal enablement into runtime profiles, handle program/component distinction, and expose stdout, stderr and exit code without changing expected outputs. The specification of its configuration and assertions is upstream in [`wasi-testsuite/doc/specification.md`](https://github.com/WebAssembly/wasi-testsuite/blob/main/doc/specification.md).

Each WASI test must run with a clean, per-test capability set and scratch directory. Include the operation-based protocol (`run`, `wait`, `read` and any other operations adopted by the pinned runner), resource cleanup, service startup/termination, client/server synchronization and bounded timeout behavior. Compare the same precompiled guest artifact against WASMDOWN and a reference runtime where both support the exact interface version.

Additional compulsory fixtures, even when absent from upstream:

- **WASI 0.1:** `proc_exit`, preopens, `fd_*`, argument/environment transfer, rights, clock and random error paths, directory/path traversal, and large file offsets.
- **WASI 0.2:** `wasi:io` streams and pollables, resource lifecycle, component-mediated CLI, and behavior of a 0.2 adapter hosted internally by the 0.3 environment.
- **WASI 0.3.2:** command world `async run`, streaming stdin/stdout, correct completion futures, system/monotonic clocks, random-byte length constraints, filesystem descriptors, DNS, TCP and UDP. WASI HTTP was REMOVED at user direction in M17 (M16 slice 5), so no HTTP fixtures or worlds are mandatory; its vendored WIT remains reference data only.
- **Async composition:** A → B → C → host, futures whose completion is delayed, producer/consumer backpressure, zero-byte streams, early close, reader/writer cancellation, dropped owner resources, and error propagation after partial output.
- **Security:** denied root/network access, symlink/TOCTOU attempts, capability inheritance boundaries, resource quotas, and correct rejection of unsupported providers instead of accidental fallback to ambient host authority.

The normative test matrix includes **each stable WIT package/interface/function in the pinned WASI release, except the M17-removed WASI HTTP surface**. A suite that covers only CLI filesystem functions is not evidence that sockets have been completed. Generate an interface-function checklist automatically from vendored WIT and map every item to executable tests or a reviewed manual conformance argument.

### 12.6 Property-based, differential and fuzz testing

Use deterministic seeds and delta-debugging/minimization for:

- **Binary/parser fuzzing:** legal and illegal LEB128; signed edge encodings; oversized vectors; duplicate/misordered sections; unknown opcodes; recursive type definitions; extremely deep nesting; corrupt WIT and component encodings. Decoder must not call `READ` on guest bytes.
- **Well-typed generation:** typed instruction sequences and structured control flow, including unusual branches and unreachable-polymorphic stacks; compare reference vs compiled modes and optionally Wasmtime.
- **Integer/float laws:** exhaustive small-domain i32 operations and boundary sets for i64; overflow, signed/unsigned comparisons, shifts/rotates, divide traps; f32/f64 signed zero, min/max, rounding and reinterpretation; explicitly permitted NaN classes.
- **Memory/table laws:** address overflow, 0/last/out-of-bounds addresses, memory64, imported/shared instance identities, growth failure and overlapping copy; compare 100% of mutated regions or authoritative hashes plus sampled positions.
- **GC and exception graphs:** random recursive type graphs, nominal/equivalence/subtyping interactions, structs, arrays, i31, cast/branch instructions, imported references, nested catch/throw/rethrow and tail-call interactions.
- **Optimization metamorphism:** compile with each pass disabled, shuffled legal pass order, or different optimization levels; equivalent observables are required.
- **Async state generation:** bounded task graphs; deliberate suspend/resume races, cancellations, stalled streams, destructor failure, recursive component chains and bounded-buffer deadlock detection.

In each fuzzer, distinguish **guest rejection**, **specified guest trap**, **resource-limit hit**, **test harness failure**, and **host crash**. Record and replay all failures. Use worker processes with OS limits for malicious binaries: an in-process Wasm guest is not a strong native-process security boundary.

### 12.7 CI validation lanes and frequency

The repository should implement the following targets as **new project commands** (not assumed to already exist):

```sh
# Proposed WASMDOWN developer targets, to be implemented:
make test-unit
make test-spec-core       # original Core .wast suite, all execution engines
make test-component       # component-model .wast + Canonical ABI corpus
make test-wasi-p1
make test-wasi-p2         # WASMDOWN compatibility corpus
make test-wasi-p3         # official runner + per-world supplemental fixtures
make test-differential
make test-security
make test-regressions
make test-all
make report-conformance
```

| Lane | Trigger | Mandatory status |
| --- | --- | --- |
| Quick | Every pull request | Source lint, binary-reader unit tests, affected `.wast`, regression corpus, codegen smoke tests |
| Core comprehensive | Every merge to main and nightly | Complete pinned Core 3.0 suite on interpreter, baseline and optimized modes; AOT sampling |
| Component/WASI | Every merge to main and nightly | Component tests; versioned WIT matrix; WASI P1/P2/P3 tests and async composition scenarios |
| Fuzz and sanitization | Scheduled nightly; prolonged weekly | Random well-typed generation, hostile input, timeouts, crash reproducibility; any crash fails lane |
| Portability | Scheduled and before release | Linux x86-64, Linux AArch64, and other platforms claimed as supported; fixed SBCL versions |
| Performance | Nightly, non-blocking during development; release gate at M6+ | Wasmtime comparison with recorded machine configuration, median/p95, compile time, allocation and memory use |
| Release candidate | Required before tag | All pinned applicable suites, zero unexplained semantic deviations, all feature-family gaps resolved, security checklist, artifact rebuild and test-report archive |

CI must publish stable artifacts: suite manifest/commit pins, normalized per-assertion JSON/JSONL, summary Markdown, minimized counterexamples, compiler/runtime/CPU information, and the exact command line for reproducing each failure. Feature tests must **not** silently convert `unsupported`, `skip`, `xfail`, timeout or harness errors into passes.

### 12.8 Machine-readable test result contract

The test runner's proposed JSON Lines (one result per assertion or fixture) should preserve at least these fields:

```json
{
  "schema": "wasmdown.test-result.v1",
  "suite": "core-spec",
  "suite_revision": "<full pinned git commit>",
  "spec_profile": "wasm-3.0",
  "engine": "optimized",
  "platform": "linux-x86_64-sbcl",
  "case_id": "i32.wast::<script-command-index>",
  "feature": "integer-arithmetic",
  "assertion": "assert_return",
  "outcome": "pass",
  "expected_failure_phase": null,
  "actual_failure_phase": null,
  "seed": null,
  "elapsed_ms": 0,
  "artifact_sha256": "<hex digest>"
}
```

This is an **illustrative schema, not a real test result**. Use an explicit enum such as `pass | fail | skip | unsupported | expected-fail | timeout | harness-error | resource-limit`, and report totals separately by suite, family, architecture and engine. `expected-fail` does not count toward full-compliance success; a resource-limit classification needs an independently reviewed justification under [Core implementation limitations](https://webassembly.github.io/spec/core/appendix/implementation.html). Publish unresolved, newly failing and previously fixed counts so that a stable overall pass percentage cannot conceal regressions.

A generated `CONFORMANCE.md` should show exactly:

- selected normative revision and WIT packages;
- numerator/denominator of **executed applicable assertions** by opcode/feature family;
- full counts and reasons for `skip`, `unsupported`, `expected-fail`, `timeout`, `harness-error` and resource-limit cases;
- missing normative feature inventory entries, **even if no official test covers them**;
- per-engine interpreter/baseline/optimized/AOT deviations;
- per-platform and per-WASI-world status;
- differential failures, fuzz crashes, non-reproducible results and outstanding security issues;
- exact reproducibility commands and build/runner hashes.

### 12.9 Conformance coverage model and hard release thresholds

Define test completion separately from normative coverage. For suite \(S\), let \(A(S)\) be the set of test assertions applicable to the pinned target and supported declared host profile; define:

\[
\operatorname{SuitePass}(S)=
\frac{\#\{t\in A(S)\mid\operatorname{result}(t)=\mathrm{pass}\}}
{\#A(S)}.
\]

Define \(F\) as the **normative inventory** of opcode semantics, type rules, binary forms, Component Model constructs and stable WIT surface functions, and \(C\subseteq F\) as the items with reviewed implementation evidence and targeted tests. Only \(C=F\) qualifies as **full feature coverage**. A green test suite with \(C\ne F\) is insufficient for the compliance label.

| Gate | Hard acceptance criteria |
| --- | --- |
| G0 — Decoder/validation | Exact failure-phase classification for malformed/invalid corpus; all parser/type-system fixtures pass; deterministic error/limit reporting |
| G1 — Executable core | Full interpreter/baseline parity for implemented scalar, memory, control-flow and import families; no host crashes |
| G2 — Core 3.0 | `SuitePass(core) = 1` for all applicable cases; zero unreviewed skip/unsupported/xfail; `C = F` for every normative Core 3.0 family; legal limits documented |
| G3 — Optimizer equivalence | Baseline vs optimized vs interpreter match on all pinned deterministic core tests and differential corpus; zero known semantic regressions; performance results published |
| G4 — Component Model | All pinned applicable Component Model tests pass and all WASI-adopted Canonical ABI features have positive/negative test coverage, including async resource life cycles |
| G5 — WASI 0.3.2 | All applicable pinned official P3 tests pass plus `C = F` for every declared stable 0.3.2 interface/world; CLI, filesystem, sockets, stream/future and nested-async tests pass; WASI HTTP is excluded (REMOVED at user direction in M17, recoverable from git history); P1/P2 claims audited separately |
| G6 — Release | G2–G5 green on declared platforms; no untriaged critical security defects; isolated hostile-input tests; signed/checksummed reproducible binaries and reports; explicit platform/resource-limit matrix |

An operational **1.0 release** claiming full WebAssembly Core 3.0 + selected published WASI 0.3.2 stable worlds requires **G2, G3, G4, G5 and G6**, with the advertised scope enumerated precisely. If any world/interface/function is omitted, the WASI claim must instead be labeled **partial**. A full *WASI 0.3.2* claim requires all mandatory adopted stable interfaces in the intended standard worlds, not a handpicked CLI subset. Passing these suites documents strong conformance evidence but does not provide a mathematical proof of an implementation's correctness.

### 12.10 Test implementation work as a milestone deliverable

| Milestone | Mandatory harness/validation deliverables |
| --- | --- |
| M0 | Version/feature lockfiles, cloned official suites, minimal `.wast` parser or test-only adapter, CI skeleton, classification schema and empty reproducible conformance report |
| M1 | Binary/text positive/negative tests, fuzz-seed corpus, malformed-vs-invalid classification, bounded parser tests |
| M2 | Full type-system assertions and recursive-subtyping fixtures; no unclassified validator mismatch |
| M3 | Stateful spec runner and reference interpreter; independent numeric/memory test oracles |
| M4 | Core tests run in interpreted + compiled modes; structured per-opcode parity results |
| M5 | All Core 3.0 feature families, official applicable assertions and feature-inventory gaps resolved; G2 |
| M6 | Per-pass optimizer differential suite, Wasmtime reference checks, benchmarking gates; G3 |
| M7 | Component suite adapter, exhaustive Canonical ABI matrix and borrow/own lifetime invariants; G4 as soon as async coverage completes |
| M8 | WASI 0.1 official adapter + explicit WASI 0.2 compatibility suite |
| M9 | Async state-machine, composition, cancellation and backpressure property suites |
| M10 | WASI 0.3 official adapter, complete stable WIT-function coverage, world-level integration and network fixtures (WASI HTTP excluded after its M17 removal); G5 |
| M11 | Hostile-input fuzz budget, capability audit, portability, reproducible artifact/release report; G6 |

At each release tag, archive `CONFORMANCE.md`, machine-readable assertion reports, suite/WIT lockfiles and license notices. This prevents later changes in a floating official suite from retroactively changing the meaning of an earlier published compliance claim.

## 13. Performance engineering and measurement

### 13.1 Optimization priorities

1. Minimize per-instruction allocations: typed locals/SSA values, avoid CLOS/message dispatch in hot opcode paths.
2. Specialize i32 and i64 word primitives; inspect SBCL disassembly to ensure machine arithmetic rather than pervasive bignums.
3. Lower CFG to `TAGBODY`/`GO` and direct local jumps; avoid recursive interpretation of block structure.
4. Inline verified small arithmetic and memory primitives; keep expensive/rare GC, exception and host functions out of hot loops.
5. Perform static bounds-range analysis and safe hoisting; specialize memory32 fast paths while retaining complete memory64.
6. Use cheap function-identity/type dispatch with cacheable indirect-call signatures.
7. Introduce target-specific SIMD after a full scalar SIMD reference implementation.
8. Compile on demand and cache FASLs; use per-function compilation to avoid pathological startup and native code sizes.

### 13.2 Benchmarks

Measure the full matrix for interpreter, baseline Lisp, optimized Lisp, and a fixed Wasmtime version. Run single-threaded, warmed functions under matched inputs on Linux x86-64 first, then AArch64; include startup separately. Use parser/CRC, integer hashing, tree/GC allocation, memory copy, irregular control flow, SIMD workloads, numerical kernels, a real Rust WASI CLI, and an asynchronous component pipeline.

Publish median, p95 and confidence intervals for throughput, plus compilation time, RSS, cache hit/miss, binary size, allocations/op and GC time. For benchmark \(b\), measure \(r_b=T_{\mathrm{Wasmtime},b}/T_{\mathrm{CL},b}\) for runtime duration (larger is better), and report geometric mean over a frozen suite:

\[G=\exp\left(\frac{1}{|B|}\sum_{b\in B}\ln r_b\right).\]

**Proposed stretch objectives, not predictions:** for selected compute-heavy kernels on SBCL/x86-64, \(G\ge 0.5\) relative to Wasmtime (runtime throughput equivalently at least 50%); at least a 5× speedup over the reference interpreter on applicable kernels; no material allocation in tight i32/i64 arithmetic loops; and less than 10% extra runtime under optional conservative checks versus the validated baseline on selected cases. Benchmark data may require redefining these goals after M3, but conformance never changes.

---

## 14. Milestone-based delivery plan

Effort is a planning estimate for *one experienced, full-time implementer* with CI access and deep Lisp/VM expertise. Milestones are dependency-ordered, with concurrency possible on tests, WIT, and host providers. Estimates are not promises and may shift substantially on component async, GC subtyping, security audits, and portability.

| ID | Effort | Deliverable | Objective exit criterion |
| --- | --- | --- | --- |
| M0 | 2–3 wk | Version manifest, pinned specs/WIT, ADRs, fixture corpus, CI, benchmark runner. | Reproducible test harness plus exact version checksum manifest. |
| M1 | 4–6 wk | Core binary reader, diagnostics, malformed-module defenses, text/WAST test adapter. | Parser and negative decoding suites pass; no reader/allocator crashes. |
| M2 | 6–9 wk | Complete core validation engine and type graph, typed IR skeleton. | Official static validation assertions pass for implemented format families. |
| M3 | 6–9 wk | Reference evaluator plus scalar/memory/table functions, imports, traps, start lifecycle. | Real Wasm 1.0/2.0 scalar programs execute; differential oracle operational. |
| M4 | 6–9 wk | SBCL baseline emitter, CFG, returns, trampoline, FASL cache, CLI. | Interpreter/compiler parity on supported core families; generated source inspectable. |
| M5 | 10–16 wk | Remaining Core 3.0 families: GC/ref typing, memory64/multi-memory, EH, complete SIMD/relaxed SIMD, profiles. | **G2:** all applicable pinned Core 3.0 tests pass, no omitted normative features. |
| M6 | 6–9 wk | Optimizer, specialized integer/float/memory fast paths; perf profiling and regression gates. | **G3:** optimized semantic parity; benchmark baseline published. |
| M7 | 8–12 wk | Component decoder/linker/type system; WIT parser, canonical ABI and resources. | Baseline component samples compose; ownership and ABI tests pass. |
| M8 | 6–10 wk | WASI 0.1 and 0.2 facades; capability-aware filesystem, clocks, random and stdio. | Legacy official suites and own 0.2 compatibility cases pass. |
| M9 | 10–16 wk | WASI 0.3.2 async component ABI: task state machine, future/stream, backpressure and wake propagation. | Cross-component suspension/cancellation and async Canonical ABI parity. |
| M10 | 8–14 wk | Complete 0.3.2 stable world providers, sockets and host adapters (WASI HTTP and its TLS adapter were REMOVED at user direction in M17). | **G5:** applicable pinned WASI 0.3 suites and component-world tests pass. |
| M11 | 6–10 wk | Security hardening, quotas, fuzzing, 2nd-architecture portability, documentation/release. | **G6:** all gates; no open critical defects; reproducible release and support matrix. |
| M12 (optional) | 8–16 wk | Threads/atomics proposal compatibility, shared memory and atomic backend. | Separate proposal-suite compliance; Core 3.0 label unchanged. |

The effort sum for M0–M11 is roughly **78–123 engineering weeks (about 18–28 engineer-months)** before ongoing maintenance, release coordination, and unanticipated architectural rework; a practical solo calendar may be about **24–36 months**. A 3-person team with distinct compiler/runtime, component/ABI, and WASI/OS ownership may compress the critical path, but not to one third because validation, async semantics, integration and fuzzing remain sequential bottlenecks.

### 14.1 Critical path

```text
M0 -> M1 -> M2 -> M3 -> M4 -> M5 -> M6
             \                    \
              \-> (early WIT) -> M7 -> M9 -> M10 -> M11
                      \             /
                       ---> M8 -----
```

### 14.2 Progressive public releases

| Release | Point of publication | Allowed public claim |
| --- | --- | --- |
| 0.1 | After M3–M4 | Experimental core subset interpreter/compiler; no full compliance claim. |
| 0.3 | After M5 | Core Wasm 3.0 conformant under published limits, if G2 fully achieved. |
| 0.5 | After M6–M8 | Optimized Core 3.0 plus partial/complete explicitly versioned legacy WASI. |
| 0.9 | After M9–M10 | WASI 0.3.2 release candidate; feature coverage documented per world. |
| 1.0 | After M11 | Core 3.0 + complete pinned Component/WASI 0.3.2 target, backed by published conformance evidence. |

---

## 15. Risks, decision records and definition of done

| Risk | Impact | Mitigation / decision |
| --- | --- | --- |
| i64 and f32/f64 optimization mismatch | Incorrect arithmetic or large performance deficit. | Reference helpers, exhaustive numeric edge tests, SBCL disassembly, no unsafe speed 3 assumptions until proven. |
| GC recursion/subtyping complexity | Invalid type acceptance or denial of service. | Separate canonical graph engine; memoized type recursion; adversarial fixtures and quotas. |
| Tail calls and async suspension | Host stack overflow or broken readiness propagation. | Explicit trampoline; yield-point CPS state machine; nested component tests. |
| Memory64 and OS VM semantics | Address truncation, OOM, host SIGSEGV. | Checked mathematical addresses, modular typed width, independently tested backends and quotas. |
| FFI/host capability escapes | Host filesystem/network compromise. | Audited OS primitives, descriptor-rooted access, empty ambient rights, process isolation for hostile guests. |
| Scope creep from unratified proposals | Never reaches complete baseline. | Freeze Core 3.0/WASI 0.3.2 snapshot; put extensions in post-1.0 modules. |
| Optimized code size/compiler latency | Runtime slower to start than competitors. | Baseline/optimized tiers, function-level compilation, source-size budgets and AOT cache. |

### 15.1 ADRs required before implementation

1. SBCL target and supported OS/CPU matrix.
2. i64 bit-pattern canonical representation.
3. v128 baseline and optional native SIMD strategy.
4. Memory32 vector vs reserved-address-space backend and memory64 allocation limits.
5. Guest exception vs host condition mechanism.
6. Cross-function tail-call trampoline ABI.
7. Compiler representation of async resumable frames.
8. Canonical resource ownership and cancellation semantics.
9. OS capability handling, TLS provider selection and threat model.
10. FASL trust/cache policy and conformance-suite version pins.

### 15.2 Definition of done

- [ ] Every normative Core 3.0 feature is implemented, including GC, exception handling, memory64, multimemory, SIMD and relaxed SIMD.
- [ ] Official Core 3.0 tests pass with no semantic skips or hidden missing feature gates, subject to documented permitted limits.
- [ ] Compiled Lisp source is inspectable; optimized and interpreter paths are differential-equivalent.
- [ ] Component Model encoding, validation, Canonical ABI, nested composition, resources and required adopted feature gates pass tests.
- [ ] WASI 0.3.2 published stable interfaces and intended standard worlds are implemented and pass applicable conformance tests.
- [ ] WASI 0.1 and 0.2 compatibility assertions are separately documented and tested.
- [ ] Host capability/security review passes; hostile programs can be run with meaningful worker-process isolation.
- [ ] Benchmarks, architecture/platform support, explicit resource limits, build instructions, pinned dependencies and release artifacts are published.

> **Recommended sequencing:** freeze the standard snapshots, prove validator/reference correctness, make Lisp compilation fast, then implement Component Model semantics and native WASI 0.3 async. Treat full compliance as a measured release gate, not a label attached to the first successful Hello World.

---

## 16. Normative references and verification sources

1. [WebAssembly Core Specification, Release 3.0 (living specification viewed 2026-10-22)](https://webassembly.github.io/spec/core/) — definitive core syntax, type validation, execution, binary and text formats.
2. [Core 3.0 Change History](https://webassembly.github.io/spec/core/appendix/changes.html) — exact feature families including GC, EH, memory64, mult-memory and relaxed SIMD.
3. [Core 3.0 Binary Values](https://webassembly.github.io/spec/core/binary/values.html) — accepts specification-permitted padded LEB128 encodings.
4. [Core 3.0 Implementation Limitations](https://webassembly.github.io/spec/core/appendix/implementation.html) — permitted resource ceilings, and the prohibition on omitting normative features.
5. [WebAssembly Specification source, reference interpreter and test suite](https://github.com/WebAssembly/spec).
6. [WebAssembly Component Model design and specification](https://github.com/WebAssembly/component-model); in particular the [Canonical ABI document](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md).
7. [WASI v0.3.2 GitHub release, 13 October 2026](https://github.com/WebAssembly/WASI/releases/tag/v0.3.2).
8. [WASI 0.3 overview: async functions, streams, futures and worlds](https://wasi.dev/releases/wasi-p3).
9. [Component Model features adopted by WASI releases](https://github.com/WebAssembly/WASI/blob/main/docs/ComponentModelFeatures.md).
10. [WASI test suite](https://github.com/WebAssembly/wasi-testsuite).
11. [WebAssembly active proposals](https://github.com/WebAssembly/proposals/blob/main/README.md) — tracks the separate threads proposal status.
12. [Steel Bank Common Lisp](https://sbcl.sourceforge.io/) — target compiler documentation and versioning.

The standards are version-pinned as of 22 October 2026. Before starting M0, record exact upstream commit hashes for both the specifications and the conformance corpus. MathJax 4 is loaded from a public CDN only for displaying formulas; the document remains readable offline with formulas visible as LaTeX source if the script is unavailable.

### Additional test-suite and tool references

- [Core `.wast` conformance test corpus](https://github.com/WebAssembly/spec/tree/main/test/core)
- [Aggregated core/proposal test mirror](https://github.com/WebAssembly/testsuite)
- [Component Model test corpus](https://github.com/WebAssembly/component-model/tree/main/test)
- [WASI test runner, adapters, branch and CLI](https://github.com/WebAssembly/wasi-testsuite/blob/main/README.md)
- [WASI JSON test specification and operation-based checks](https://github.com/WebAssembly/wasi-testsuite/blob/main/doc/specification.md)
- [WABT test-only `.wast` to JSON conversion](https://github.com/WebAssembly/wabt/blob/main/docs/wast2json.md)
- [wasm-tools test tooling and its suite runner](https://github.com/bytecodealliance/wasm-tools/blob/main/CONTRIBUTING.md)
- [Permitted WebAssembly implementation limits](https://webassembly.github.io/spec/core/appendix/implementation.html)

**Document integrity:** This Markdown retains the implementation architecture and roadmap of revision 1.0. §12 was superseded with a larger test/validation contract. All CLI commands and test report schemas labeled “proposed” are design requirements, not assertions about an existing implementation.
