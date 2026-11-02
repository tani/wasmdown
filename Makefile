# WASMDOWN developer commands (subset of docs/design/PLAN.md §12.7 targets).

SBCL ?= sbcl
ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
LISP_FILES := $(wildcard src/*.lisp)

.PHONY: all test test-unit test-spec-core test-component test-component-census report-component-compiled \
        test-backend-parity report-backend-parity \
        report-backend-inventory test-backend-inventory \
        report-backends-bench \
        test-upstream test-wasi-preview1 test-wasi-preview2 test-wasi \
        report-conformance p3-fixtures test-wasi-preview3 verify \
        test-property test-async-property test-async-property-negative \
        test-p1-repeat setup-network-tests test-wasi-network test-wasi-network-reference \
        setup-wasi-toolchain build-toolchain-guest test-toolchain-guest \
        setup-curl-wasm test-curl-wasm setup-p2-guests test-p2-guests \
        record-p2 record-p3 record-p3-compiled record-wasi-compiled \
        record-release verify-release \
        clean \
        load-system repl wasi-fixtures

# Reproducible quick gate: the deterministic lanes that must be green on
# any change. Compiled large-guest lanes stay report-only. ~2.5 min.
verify: test-unit test-component test-backend-parity test-backend-inventory \
	test-wasi test-wasi-preview3 test-property test-async-property test-toolchain-guest test-p2-guests
	$(SBCL) --script tools/build-p3-fixtures.lisp --check
	@echo "verify: all quick gates green"

# Differential property/fuzz lane: generated core modules run under both
# backends; results, trap kinds and observable memory must agree. A failure
# prints its seed for reproduction. ITERATIONS overrides the default.
ITERATIONS ?= 2000
test-property:
	$(SBCL) --control-stack-size 32 --dynamic-space-size 4096 --script tools/fuzz-backends.lisp --iterations $(ITERATIONS)

# Pinned Rust nightly + wasm32-wasip3 guest toolchain (git-ignored .cache/).
setup-wasi-toolchain:
	tools/setup-wasi-toolchain.sh

# Build tests/toolchain/wasi-guest and pin the generated component hash.
build-toolchain-guest:
	tools/build-toolchain-guest.sh

# Generated @0.3-line command guest: exact stdout in both backends.
test-toolchain-guest:
	python3 tools/run-toolchain-guest.py

# curl built as a WASI 0.3 component with wasi-sdk 34 (dev/test only).
setup-curl-wasm:
	tools/setup-curl-wasm.sh

test-curl-wasm:
	python3 tools/test-curl-wasm.py

# Real WASI 0.2 guests (sockets + HTTP proxy) through wasi-sdk/cargo-component.
setup-p2-guests:
	tools/setup-p2-guests.sh

test-p2-guests:
	python3 tools/test-p2-guests.py

# Host environment for the upstream network cases (http/sockets):
# pinned wasi-testsuite checkout + reference Wasmtime under .cache/.
setup-network-tests:
	tools/setup-network-tests.sh

# Upstream network subset through the wasmdown adapter (http + sockets).
# Requires the checkout selected by WASI_TESTSuite_DIR (or the setup target).
test-wasi-network:
	python3 tools/run-wasi-network-tests.py

# Reference-runtime smoke for the same subset: proves the host loopback/HTTP
# harness independently of wasmdown. Report-only (upstream Wasmtime may have
# its own failures); requires WASMTIME from the setup target.
test-wasi-network-reference:
	python3 tools/run-wasi-network-tests.py --reference

# Deterministic async property lane (docs/design/PLAN.md §9.3/F4): the REAL
# task/subtask, stream/future/cancel and host-pending primitives driven by
# fixed xorshift64* seeds (no wall-clock sleeps), plus the fake-reactor step
# protocol, the backpressure admission gate and the bounded-executor audit.
# A gate: any violated exactly-once / monotone-state / work-accounting
# invariant exits non-zero. Override the corpus with ASYNC_SEEDS/ASYNC_REPEATS.
ASYNC_SEEDS ?= 1,2,3,5,8,13,21,34
ASYNC_REPEATS ?= 3
test-async-property:
	$(SBCL) --control-stack-size 32 --dynamic-space-size 2048 --script tools/run-async-property.lisp --seeds $(ASYNC_SEEDS) --repeats $(ASYNC_REPEATS)

# Negative control for the async property gate: deliberately double-delivers
# a host-pending continuation; the invariant checkers MUST report it. The
# target is green only when the injected violation was detected.
test-async-property-negative:
	$(SBCL) --control-stack-size 32 --dynamic-space-size 2048 --script tools/run-async-property.lisp --negative-control

# Canonical P2 interpret artifacts for the release matrix.
record-p2:
	python3 tools/run-wasi-suite.py --abi preview2 --output-dir tests/wasi-results/p2-interpret

# Record the P3 lane artifacts the release matrix checks (fast).
record-p3:
	python3 tools/run-wasi-suite.py --abi preview3 --output-dir tests/wasi-results/p3-interpret

record-p3-compiled:
	python3 tools/run-wasi-suite.py --abi preview3 --backend compiled --output-dir tests/wasi-results/p3-compiled

# P2 behaviour lane under the compiled backend (~37 min at ~200 s/case).
# This is the expensive row of the release matrix; it is a report run.
record-wasi-compiled:
	python3 tools/run-wasi-suite.py --abi preview2 --backend compiled --heap-mb 8192 --output-dir tests/wasi-results/compiled-preview2

# Rebuild every recorded artifact the release matrix reads (the compiled P2
# row is the long one; comment it out for a quick refresh).
record-release: report-backend-parity test-component report-component-compiled \
	test-wasi record-p2 record-p3 record-p3-compiled report-backends-bench \
	report-backend-inventory
	@echo "record-release: artifacts regenerated (compiled P2 row: make record-wasi-compiled)"

# Check the recorded release matrix: every row present, green and from HEAD.
verify-release:
	python3 tools/check-release-matrix.py

# P1 acceptance repeated in fresh processes (the plan's "P1 guest tests pass
# twice"). Interpret mode; the compiled form is report evidence, not a repeat
# gate. RUNS overrides the default.
RUNS ?= 3
test-p1-repeat:
	@for i in $$(seq 1 $(RUNS)); do \
	  echo "P1 acceptance run $$i/$(RUNS)"; \
	  python3 tools/wasi-preview1-acceptance.py || exit 1; \
	done

all: test

load-system:
	$(SBCL) --non-interactive \
	  --eval '(require :asdf)' \
	  --eval "(pushnew #P\"$(ROOT)/\" asdf:*central-registry*)" \
	  --eval '(asdf:load-system :wasmdown)' \
	  --eval '(quit)'

test: test-unit

test-unit:
	$(SBCL) --script tools/run-tests.lisp

# Core spec-script conformance lane (docs/design/PLAN.md §12.7): run the vendored
# official suite subset through the stateful WAST runner. Unsupported
# feature files are classified, never silently passed.
test-spec-core:
	$(SBCL) --control-stack-size 16 --dynamic-space-size 2048 --script tools/run-spec-core.lisp

# Backend parity gate: every pinned core script once through :interpret and
# once through :compiled, paired by case id. Exits non-zero on any divergence,
# compiled compile refusal, timeout/harness error, or command-count mismatch.
# Baseline compilation needs more heap than the interpreter-only lane.
test-backend-parity:
	$(SBCL) --control-stack-size 16 --dynamic-space-size 8192 --script tools/run-backend-parity.lisp --mode gate

# Backend parity report (always exits zero): regenerates
# docs/conformance/backends.md plus the JSONL/summary artifacts.
report-backend-parity:
	$(SBCL) --control-stack-size 16 --dynamic-space-size 8192 --script tools/run-backend-parity.lisp --mode report --commit

# Backend cost bench: decode / validate / instantiate(+compile) / run for
# synthetic kernels and an optional real module, medians over --repeat runs.
# Writes docs/conformance/performance.md and tests/bench-backends.json.
report-backends-bench:
	$(SBCL) --control-stack-size 32 --dynamic-space-size 4096 --script tools/bench-backends.lisp --repeat 3 --commit --json tests/bench-backends.json

# Backend/ABI inventory: enumerate the pinned WASI surfaces from the runtime's
# real registration tables and reconcile them with the pinned census; writes
# tests/backend-inventory.json and docs/conformance/inventory.md.
report-backend-inventory:
	python3 tools/backend-inventory.py

test-backend-inventory:
	python3 tools/backend-inventory.py --check

# Component suite lane (docs/design/PLAN.md §12.7, M7): tests/component/{binary,
# validation} minus nyi.txt through the component-aware WAST runner.
# Gate with honest per-command classification and a census: exits
# non-zero on any fail, skip, timeout, harness error, or census drift.
test-component:
	$(SBCL) --control-stack-size 16 --dynamic-space-size 2048 --script tools/run-component-suite.lisp

# WASI Preview1 end-to-end lane: builds nothing (fixtures are pinned
# artifacts), runs the REAL CLI over the SQLite shell fixtures with
# exact stdout/status assertions plus the host-side denial cases.
test-wasi-preview1:
	python3 tools/wasi-preview1-acceptance.py

# WASI 0.2 behaviour lane (docs/design/PLAN.md 12.7): one fresh SBCL guest process
# per case against the real CLI. Exact stdout/status, on-disk
# persistence across processes, cross-ABI file compatibility, denial.
test-wasi-preview2:
	python3 tools/run-wasi-suite.py --abi preview2

# Both released ABIs. The backend follows WASMDOWN_API_MODE exactly as
# the runtime does, so `WASMDOWN_API_MODE=compiled make test-wasi`
# exercises the compiled backend.
test-wasi:
	python3 tools/run-wasi-suite.py --abi all

# Component suite under the BASELINE backend: the same corpus with every
# defined core function compiled before its start section. Report mode (a
# refusal is data here); writes tests/component-compiled/ and
# tests/component-compiled-results.jsonl.
report-component-compiled:
	$(SBCL) --control-stack-size 16 --dynamic-space-size 8192 --script tools/run-component-suite.lisp --backend compiled --mode report

# Component-lane census (docs/design/PLAN.md §12.7): RUNS full lane executions in
# fresh processes, each required to be GATE: GREEN with the pinned
# 1702-command denominator. A shrinking denominator or any fail/herr/
# timeout/skip fails the census. Default 30 runs; override with RUNS=n.
RUNS ?= 30
test-component-census:
	bash tools/run-component-census.sh $(RUNS)

# Upstream WebAssembly/testsuite adapter lane (dev/discovery only): the
# mirror is fetched at an exact commit and measured through the WAST
# runner into tests/upstream-results.jsonl. Never a release gate.
test-upstream:
	python3 tools/run-upstream-testsuite.py

# Hand-authored WASI 0.3.2 command fixture (no @0.3.2 toolchain here).
# Writes tests/fixtures/wasi/p3-guest.wasm + P3-CHECKSUMS; --check verifies.
p3-fixtures:
	$(SBCL) --script tools/build-p3-fixtures.lisp

# WASI 0.3.2 guest lane: the pinned hand-authored command, real CLI, one
# case. It is NOT part of --abi all (released ABIs only) and requires the
# fixture above; --check fails when the checked-in hash does not match.
test-wasi-preview3: p3-fixtures
	python3 tools/run-wasi-suite.py --abi preview3

# Development-only fixture build (needs the pinned Nix toolchain).
wasi-fixtures:
	nix shell nixpkgs#zig nixpkgs#wasm-tools --command bash tools/build-wasi-fixtures.sh

report-conformance: test-spec-core
	@echo "docs/conformance/core.md regenerated by test-spec-core"

repl:
	$(SBCL) --eval '(require :asdf)' \
	  --eval "(pushnew #P\"$(ROOT)/\" asdf:*central-registry*)" \
	  --eval '(asdf:load-system :wasmdown)'

# Upstream WASI testsuite lane (docs/design/PLAN.md §9.3 F1): the registered
# adapter adapters/wasmdown.py runs the pinned precompiled guests
# (WebAssembly/wasi-testsuite, commit in specs/lock.toml) and writes
# tests/wasi-upstream-results.json + tests/wasi-upstream-summary.md. The tree
# is never vendored: point WASI_TESTSuite_DIR (or WASI_TESTSUITE_DIR) at a
# checkout whose HEAD is the pin. Without one the target exits 2 with the
# recorded blocker; with one it exits 1 while the lane records failures (this
# is evidence, never a release gate, and it is not part of `verify`).
.PHONY: test-wasi-upstream
test-wasi-upstream:
	python3 tools/run-wasi-testsuite.py
