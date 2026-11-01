#!/usr/bin/env python3
"""WASI Preview1 end-to-end acceptance lane (development-only).

Runs the REAL wasmdown CLI over the pinned SQLite 3.54.1 shell fixtures
(``tests/fixtures/wasi/sqlite3-p1.wasm``) and asserts exact guest
streams, exit statuses and host-side denial effects. Nothing here is an
external executor: every guest runs in wasmdown through the
``wasi_snapshot_preview1`` facade.

Cases
  smoke            in-memory query, exact stdout "42"
  persistence      create/insert/query inside an explicit preopen,
                   then a SECOND process reopens the same file
  rollback         a rolled-back transaction leaves no rows
  bail             a UNIQUE violation under -bail exits nonzero
  no-preopen       no ambient filesystem: the database cannot be opened
  read-only        a read-only preopen denies creation/writing
  escape           a path escaping the preopen root is refused

Usage: tools/wasi-preview1-acceptance.py [--verbose]
       [--backend interpret|compiled] [--heap-mb N]

--backend compiled runs the same cases with WASMDOWN_API_MODE=compiled:
every guest core is compiled before its start section, which needs a much
larger SBCL heap (~8 GiB for the 6 MB SQLite fixture) and roughly 200 s
of compile time per fresh process. The lane stays exact about stdout,
status and host effects; nothing is retried on the other backend.
Exit status: 0 when every case passes.
"""
import os
import shutil
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FIXTURE = os.path.join(ROOT, "tests", "fixtures", "wasi", "sqlite3-p1.wasm")
LAUNCHER = os.path.join(ROOT, "tools", "wasmdown.lisp")
VERBOSE = "--verbose" in sys.argv[1:]


def _arg(flag, default):
    argv = sys.argv[1:]
    if flag in argv and argv.index(flag) + 1 < len(argv):
        return argv[argv.index(flag) + 1]
    return default


BACKEND = _arg("--backend", os.environ.get("WASMDOWN_BACKEND", "interpret"))
HEAP_MB = int(_arg("--heap-mb", "8192" if BACKEND == "compiled" else "0"))
CASE_TIMEOUT = 900 if BACKEND == "compiled" else 240
if BACKEND not in ("interpret", "compiled"):
    print(f"unknown --backend {BACKEND!r}")
    sys.exit(2)

failures = []


def run_cli(args, stdin_text, env=None, timeout=None):
    heap = ["--dynamic-space-size", str(HEAP_MB)] if HEAP_MB else []
    command = (["sbcl"] + heap
               + ["--script", LAUNCHER, "run", FIXTURE, "--wasi"] + args)
    environment = dict(os.environ)
    if BACKEND == "compiled":
        environment["WASMDOWN_API_MODE"] = "compiled"
    if env:
        environment.update(env)
    timeout = CASE_TIMEOUT if timeout is None else timeout
    proc = subprocess.run(command, input=stdin_text, text=True,
                          capture_output=True, timeout=timeout, cwd=ROOT,
                          env=environment)
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


def main():
    for path, what in ((FIXTURE, "sqlite3-p1.wasm"), (LAUNCHER, "tools/wasmdown.lisp")):
        if not os.path.exists(path):
            print(f"PREREQUISITE MISSING: {what} ({path})")
            print("Run tools/build-wasi-fixtures.sh first.")
            return 1

    work = tempfile.mkdtemp(prefix="wmd-p1-")
    empty_init = os.path.join(work, "empty.sql")
    with open(empty_init, "w"):
        pass
    db = os.path.join(work, "test.db")
    preopen = [f"--dir", f"{work}::/work", "--env", "HOME=/work"]

    try:
        # 1. smoke: in-memory query
        p = run_cli(["--", ":memory:"], ".bail on\nSELECT 6*7;\n")
        check("smoke/stdout-has-42", "42" in p.stdout, p.stdout)
        check("smoke/status", p.returncode == 0, p.returncode)

        # 2. persistence: exact rows, then reopen in a fresh process
        script = (".bail on\n"
                  "CREATE TABLE t(id INTEGER PRIMARY KEY,name TEXT UNIQUE,score REAL);\n"
                  "INSERT INTO t(name,score) VALUES('alpha',1.5),('beta',2.25),('gamma',9.75);\n"
                  "SELECT count(*), sum(score) FROM t;\n"
                  "SELECT group_concat(name,',') FROM (SELECT name FROM t ORDER BY name);\n"
                  "PRAGMA integrity_check;\n")
        p = run_cli(preopen + ["--", "-batch", "-bail", "-init",
                              "/work/empty.sql", "/work/test.db"], script)
        expected = "3|13.5\nalpha,beta,gamma\nok\n"
        check("persistence/stdout", p.stdout == expected,
              f"{p.stdout!r} != {expected!r}")
        check("persistence/status", p.returncode == 0, p.returncode)
        check("persistence/db-created", os.path.getsize(db) > 0)

        query = (".bail on\n"
                 "SELECT count(*), sum(score) FROM t;\n"
                 "SELECT group_concat(name,',') FROM (SELECT name FROM t ORDER BY name);\n"
                 "PRAGMA integrity_check;\n")
        p = run_cli(preopen + ["--", "-batch", "-bail", "-init",
                              "/work/empty.sql", "/work/test.db"], query)
        check("reopen/stdout", p.stdout == "3|13.5\nalpha,beta,gamma\nok\n", p.stdout)
        check("reopen/status", p.returncode == 0, p.returncode)

        # 3. rollback leaves no rows
        rollback = (".bail on\n"
                    "BEGIN; INSERT INTO t(name,score) VALUES('delta',4); ROLLBACK;\n"
                    "SELECT count(*) FROM t;\n")
        p = run_cli(preopen + ["--", "-batch", "-bail", "-init",
                              "/work/empty.sql", "/work/test.db"], rollback)
        check("rollback/count", p.stdout.strip() == "3", p.stdout)

        # 4. uniqueness violation under -bail is a nonzero exit
        bail = (".bail on\nINSERT INTO t(name,score) VALUES('alpha',9);\n"
                "SELECT 'unreachable';\n")
        p = run_cli(preopen + ["--", "-batch", "-bail", "-init",
                              "/work/empty.sql", "/work/test.db"], bail)
        check("bail/status", p.returncode == 1, p.returncode)
        check("bail/no-second-statement", "unreachable" not in p.stdout, p.stdout)

        # 5. no preopen: no ambient filesystem
        p = run_cli(["--", "-batch", "/work/test.db"], "SELECT 1;\n")
        check("no-preopen/denied", p.returncode != 0 or "unable to open" in p.stderr,
              (p.returncode, p.stderr))

        # 6. read-only authority denies writes
        ro_dir = os.path.join(work, "ro")
        os.makedirs(ro_dir, exist_ok=True)
        p = run_cli(["--dir", f"{ro_dir}::/ro:ro", "--env", "HOME=/ro",
                     "--", "-batch", "-bail", "/ro/x.db"], "CREATE TABLE q(a);\n")
        check("read-only/denied", os.path.exists(os.path.join(ro_dir, "x.db")) is False,
              os.listdir(ro_dir))

        # 7. escaping the root is refused, and nothing is created outside
        outside = os.path.join(work, "outside.db")
        p = run_cli(preopen + ["--", "-batch", "/work/../outside.db"], "SELECT 1;\n")
        check("escape/denied", not os.path.exists(outside), outside)
        check("escape/message", "unable to open" in p.stderr, p.stderr)
    finally:
        if not VERBOSE:
            shutil.rmtree(work, ignore_errors=True)

    if failures:
        print(f"\n{len(failures)} acceptance case(s) failed: {', '.join(failures)}")
        return 1
    print("\nWASI Preview1 acceptance: all cases passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
