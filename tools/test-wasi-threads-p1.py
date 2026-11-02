#!/usr/bin/env python3
"""WASI Preview1 wasi-threads spawn lane (development-only).

Compiles the hand-authored WAT fixtures in tests/fixtures/wasi-threads/
with the pinned wasm-tools, then runs each module through the REAL
wasmdown CLI (`run ... --wasi --threads`) and asserts the observable
result:

  spawn-shared-memory  two children, one shared memory: the child sees
                       the host TID at its opaque start_arg and the
                       parent observes what the child wrote (stdout ok)
  spawn-wasi-alias     the proposal/wasi-libc ABI spelling
                       `wasi' / `thread-spawn' (start_arg 777)
  spawn-no-start-export
                       no wasi_thread_start export -> negative spawn
                       error observed by the guest (stdout neg)
  child-exit           proc_exit(99) in a child terminates the main
                       thread parked in memory.atomic.wait32 -> status 99
  child-trap           a trap in a child aborts the parked main thread
                       -> status 1, no hang
  main-return          returning from _start terminates a waiting child
                       -> status 0, no hang
  threads-off          without --threads the shared-memory module is
                       rejected (default behavior unchanged)
  pthread-create       a REAL wasi-libc pthread guest built with
                       wasi-sdk's wasm32-wasip1-threads target: pthread_create
                       reaches wasi_thread_spawn, the child writes an atomic
                       flag in shared memory, pthread_join completes
                       (skipped when the SDK target is unavailable)

Every case runs under a wall-clock timeout, so a broken termination path
shows up as a bounded failure instead of a hung test run. WASMDOWN_API_MODE
is inherited, so the same lane runs the compiled backend.

Usage: tools/test-wasi-threads-p1.py [--verbose]
Exit status: 0 when every case passes.
"""
import glob
import os
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LAUNCHER = os.path.join(ROOT, "tools", "wasmdown.lisp")
FIXTURES = os.path.join(ROOT, "tests", "fixtures", "wasi-threads")
VERBOSE = "--verbose" in sys.argv[1:]
TIMEOUT = int(os.environ.get("WASI_THREADS_TIMEOUT", "60"))

failures = []


def find_wasm_tools():
    env = os.environ.get("WASM_TOOLS")
    if env:
        return env
    matches = sorted(glob.glob(os.path.join(
        ROOT, ".cache", "tools", "wasm-tools-*", "wasm-tools")))
    for path in reversed(matches):
        if os.access(path, os.X_OK):
            return path
    return None


def find_wasi_clang():
    env = os.environ.get("WASI_SDK_CLANG")
    if env:
        return env
    matches = sorted(glob.glob(os.path.join(
        ROOT, ".cache", "tools", "wasi-sdk-*", "bin", "clang")))
    for path in reversed(matches):
        if os.access(path, os.X_OK):
            return path
    return None


def compile_c(clang, name, out_dir):
    source = os.path.join(FIXTURES, name + ".c")
    target = os.path.join(out_dir, name + ".wasm")
    # The wasip1-threads libc is built with atomics; the shared memory
    # needs room for pthread stacks, so pin a 2 MiB initial=max heap.
    proc = subprocess.run(
        [clang, "--target=wasm32-wasip1-threads", "-O2",
         "-Wl,--initial-memory=2097152", "-Wl,--max-memory=2097152",
         source, "-o", target],
        capture_output=True, text=True, timeout=TIMEOUT)
    if proc.returncode != 0:
        raise RuntimeError(f"clang {name}: {proc.stderr}")
    return target


def compile_wat(wasm_tools, name, out_dir):
    source = os.path.join(FIXTURES, name + ".wat")
    target = os.path.join(out_dir, name + ".wasm")
    proc = subprocess.run([wasm_tools, "parse", source, "-o", target],
                          capture_output=True, text=True, timeout=TIMEOUT)
    if proc.returncode != 0:
        raise RuntimeError(f"wasm-tools parse {name}: {proc.stderr}")
    validate = subprocess.run(
        [wasm_tools, "validate", "--features", "threads", target],
        capture_output=True, text=True, timeout=TIMEOUT)
    if validate.returncode != 0:
        raise RuntimeError(f"wasm-tools validate {name}: {validate.stderr}")
    return target


def run_cli(wasm, threads=True, args=()):
    command = (["sbcl", "--script", LAUNCHER, "run", wasm, "--wasi"]
               + (["--threads"] if threads else []) + list(args))
    try:
        proc = subprocess.run(command, capture_output=True, text=True,
                              timeout=TIMEOUT, cwd=ROOT)
    except subprocess.TimeoutExpired:
        if VERBOSE:
            print(f"$ {' '.join(command)}\n  TIMEOUT after {TIMEOUT}s")
        return None
    if VERBOSE:
        print(f"$ {' '.join(command)}\n  status={proc.returncode}")
        print(f"  stdout={proc.stdout!r}\n  stderr={proc.stderr!r}")
    return proc


def check(name, condition, detail=""):
    if condition:
        print(f"PASS {name}")
    else:
        print(f"FAIL {name}: {detail}")
        failures.append(name)


def expect(name, wasm, status, stdout=None, threads=True):
    proc = run_cli(wasm, threads=threads)
    if proc is None:
        check(name, False, f"hung (no exit within {TIMEOUT}s)")
        return
    if status is not None:
        check(f"{name}/status", proc.returncode == status,
              f"expected {status}, got {proc.returncode}; "
              f"stdout={proc.stdout!r} stderr={proc.stderr!r}")
    if stdout is not None:
        check(f"{name}/stdout", proc.stdout == stdout,
              f"{proc.stdout!r} != {stdout!r}")


def main():
    wasm_tools = find_wasm_tools()
    if not wasm_tools:
        print("PREREQUISITE MISSING: wasm-tools (.cache/tools/wasm-tools-*)")
        return 1
    if not os.path.exists(LAUNCHER):
        print(f"PREREQUISITE MISSING: {LAUNCHER}")
        return 1

    with tempfile.TemporaryDirectory(prefix="wmd-wasi-threads-") as work:
        wasm = {}
        for name in ("spawn-shared-memory", "spawn-wasi-alias",
                     "spawn-no-start-export", "child-exit", "child-trap",
                     "main-return"):
            try:
                wasm[name] = compile_wat(wasm_tools, name, work)
            except Exception as exc:  # noqa: BLE001 - report as a case
                check(f"compile/{name}", False, str(exc))

        if "spawn-shared-memory" in wasm:
            expect("spawn-shared-memory", wasm["spawn-shared-memory"],
                   0, "ok\n")
        if "spawn-wasi-alias" in wasm:
            expect("spawn-wasi-alias", wasm["spawn-wasi-alias"], 0, "ok\n")
        if "spawn-no-start-export" in wasm:
            expect("spawn-no-start-export",
                   wasm["spawn-no-start-export"], 0, "neg\n")
        if "child-exit" in wasm:
            expect("child-exit", wasm["child-exit"], 99)
        if "child-trap" in wasm:
            proc = run_cli(wasm["child-trap"])
            if proc is None:
                check("child-trap", False,
                      f"hung (no exit within {TIMEOUT}s)")
            else:
                check("child-trap/status", proc.returncode == 1,
                      f"expected 1, got {proc.returncode}")
                check("child-trap/diagnostic",
                      "trap" in (proc.stderr or "").lower(),
                      proc.stderr)
        if "main-return" in wasm:
            expect("main-return", wasm["main-return"], 0, "")
        clang = find_wasi_clang()
        if clang:
            try:
                wasm["pthread-create"] = compile_c(
                    clang, "pthread-create", work)
            except Exception as exc:  # noqa: BLE001 - report as a case
                check("compile/pthread-create", False, str(exc))
            if "pthread-create" in wasm:
                expect("pthread-create", wasm["pthread-create"], 0,
                       "thread:42\n")
        else:
            print("SKIP pthread-create (no cached wasi-sdk wasm32-wasip1-threads target)")
        # The gate: the same shared-memory module is rejected without
        # --threads (decode/validation error, never a guest run).
        if "spawn-shared-memory" in wasm:
            proc = run_cli(wasm["spawn-shared-memory"], threads=False)
            if proc is None:
                check("threads-off", False,
                      f"hung (no exit within {TIMEOUT}s)")
            else:
                check("threads-off/status", proc.returncode != 0,
                      f"expected nonzero, got {proc.returncode}")
                check("threads-off/no-guest-output", proc.stdout == "",
                      proc.stdout)

    if failures:
        print(f"--- {len(failures)} failing case(s): {', '.join(failures)}")
        return 1
    print("wasi-threads Preview1 lane: all cases passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
