#!/bin/bash
# ASYNC PER-FILE SCAN — one fixture per SBCL process, 25s cap, so a
# hang is attributed to a file. SERIAL.
cd /home/tani/ghq/github.com/tani/wasmdown
for f in tests/component/async/*.wast; do
  timeout 25 sbcl --control-stack-size 16 --dynamic-space-size 2048 --script tools/probe.lisp "$f" > /tmp/pf.log 2>&1
  rc=$?
  if [ $rc -ne 0 ]; then echo "HANG/ERR rc=$rc $f"; fi
  if grep -q "FAIL" /tmp/pf.log; then echo "FAIL $f: $(grep -c FAIL /tmp/pf.log)"; fi
done
echo done
