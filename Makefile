# WASMDOWN developer commands (subset of docs/design/PLAN.md §12.7 targets).

SBCL ?= sbcl
ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
LISP_FILES := $(wildcard src/*.lisp)

.PHONY: all test test-unit test-spec-core test-component test-component-census report-component-baseline \
        test-backend-parity report-backend-parity \
        report-backend-inventory test-backend-inventory \
        report-backends-bench \
        test-upstream test-wasi-preview1 test-wasi-preview2 test-wasi \
        report-conformance p3-fixtures test-wasi-preview3 \
        clean \
        load-system repl wasi-fixtures

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
# once through :baseline, paired by case id. Exits non-zero on any divergence,
# baseline compile refusal, timeout/harness error, or command-count mismatch.
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
# the runtime does, so `WASMDOWN_API_MODE=baseline make test-wasi`
# exercises the baseline backend.
test-wasi:
	python3 tools/run-wasi-suite.py --abi all

# Component suite under the BASELINE backend: the same corpus with every
# defined core function compiled before its start section. Report mode (a
# refusal is data here); writes tests/component-baseline/ and
# tests/component-baseline-results.jsonl.
report-component-baseline:
	$(SBCL) --control-stack-size 16 --dynamic-space-size 8192 --script tools/run-component-suite.lisp --backend baseline --mode report

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
