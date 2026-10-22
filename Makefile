# WASMDOWN developer commands (subset of PLAN.md §12.7 targets).

SBCL ?= sbcl
ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
LISP_FILES := $(wildcard src/*.lisp)

.PHONY: all test test-unit clean load-system repl

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

clean:
	@echo "nothing to clean"

repl:
	$(SBCL) --eval '(require :asdf)' \
	  --eval "(pushnew #P\"$(ROOT)/\" asdf:*central-registry*)" \
	  --eval '(asdf:load-system :wasmdown)'
