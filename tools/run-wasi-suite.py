#!/usr/bin/env python3
"""WASI conformance lane: Preview1 + WASI 0.2 (docs/design/PLAN.md §12.7/§12.9).

Runs a manifest of behavioural cases against the REAL CLI, one fresh
SBCL guest process per case, and emits per-case JSONL plus a summary.
This is the lane the plan requires for the two released ABIs, with the
deliberate properties:

  * every case EXECUTES the guest (no mock, no host substitute);
  * a case that cannot run (missing fixture, timeout, harness error) is
    a FAILURE, never a skip or an empty green lane;
  * repetitions start fresh processes, and there is no retry-to-green;
  * the outcome is a gate: any non-pass result exits non-zero.

Usage:
    python3 tools/run-wasi-suite.py --abi preview2
    python3 tools/run-wasi-suite.py --abi all --repeat 3 \
        --output-dir tests/wasi-results/final

Backend selection mirrors the runtime: `--backend compiled` sets
WASMDOWN_API_MODE=compiled for the guest processes (interpret is the
default when the variable is absent).

The manifest lives in this file (CASES) because it is small, explicit
and reviewable; each case names the exact binary, argv, stdin, the
preopens it is granted and the observable result. The `group` field
gives cases a shared work directory and a fixed order (a case may then
re-read a database an earlier case wrote).
"""

import argparse
import hashlib
import json
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = pathlib.Path(__file__).resolve().parent.parent
FAILURE_DIR = None  # set by main() to <output-dir>/failures
FIXTURES = ROOT / "tests" / "fixtures" / "wasi"


def _git(*args):
    try:
        out = subprocess.run(("git",) + args, cwd=ROOT, capture_output=True,
                             text=True, check=False)
        return out.stdout.strip() or "unknown"
    except OSError:
        return "unknown"


def source_identity():
    """Commit, tree and cleanliness of the tested tree, recorded with every
    result so a report cannot be attributed to the wrong commit."""
    return {
        "source_commit": _git("rev-parse", "HEAD"),
        "source_tree": _git("rev-parse", "HEAD^{tree}"),
        "clean_tree": _git("status", "--porcelain") in ("", "unknown"),
    }
P1 = "sqlite3-p1.wasm"
P2 = "sqlite3-p2.wasm"
# WASI 0.3.2 (@0.3.2 imports, Component Model async) command guests.
# Ten hand-authored guests are pinned (tools/build-p3-fixtures.lisp and
# P3-CHECKSUMS); no toolchain in the development environment produces the
# 0.3.2 component ABI, so upstream-toolchain integration is still absent.
# The lane selects preview3 ONLY when asked, and a missing fixture is a
# FAILURE (never a skip), so nothing can pass silently.
P3 = "p3-guest.wasm"
P3_IMPORT = "p3-import-guest.wasm"
P3_CLOCK = "p3-clock-guest.wasm"
P3_ASYNC = "p3-async-guest.wasm"
P3_STREAM = "p3-stream-guest.wasm"
P3_CANCEL = "p3-cancel-guest.wasm"
P3_RANDOM = "p3-random-guest.wasm"
P3_DEADLINE = "p3-deadline-guest.wasm"
P3_CLI = "p3-cli-guest.wasm"
P3_FS = "p3-fs-guest.wasm"

# ---- the cases -----------------------------------------------------
# env                   : `K=V' pairs passed with --env (a `{fixture}'
#                         placeholder expands to the fixture path, i.e.
#                         the argv[0] the runner supplies)
# expect.stdout_equals  : exact stdout (list of lines, joined by \n, one
#                         trailing newline tolerated)
# expect.stdout_contains: every string must appear in stdout
# expect.exit           : exact exit status
# expect.files          : paths (relative to the case work dir) that
#                         must EXIST (and be non-empty)
# expect.absent        : paths that must NOT exist
CASES = [
    # ---------------- WASI 0.2 ----------------
    {
        "id": "p2/argv-version",
        "abi": "preview2",
        "fixture": P2,
        "argv": ["-version"],
        "stdin": "",
        "expect": {"stdout_contains": ["3.54.1"], "exit": 0},
    },
    {
        "id": "p2/select-memory",
        "abi": "preview2",
        "fixture": P2,
        "argv": ["-batch", ":memory:"],
        "stdin": ".bail on\nSELECT 6*7;\n",
        "expect": {"stdout_equals": ["42"], "exit": 0},
    },
    {
        "id": "p2/crud-memory",
        "abi": "preview2",
        "fixture": P2,
        "argv": ["-batch", ":memory:"],
        "stdin": (
            ".bail on\n"
            "CREATE TABLE t(id INTEGER PRIMARY KEY,name TEXT UNIQUE,score REAL);\n"
            "INSERT INTO t(name,score) VALUES ('alpha',1.5),('beta',2.25),"
            "('gamma',9.75);\n"
            "SELECT count(*),sum(score) FROM t;\n"
            "SELECT group_concat(name, ',') FROM "
            "(SELECT name FROM t ORDER BY name);\n"
        ),
        "expect": {
            "stdout_equals": ["3|13.5", "alpha,beta,gamma"],
            "exit": 0,
        },
    },
    {
        "id": "p2/disk-create",
        "abi": "preview2",
        "fixture": P2,
        "group": "p2disk",
        "preopen": ["/work"],
        "argv": ["-batch", "/work/a.db"],
        "stdin": (
            ".bail on\n"
            "CREATE TABLE t(id INTEGER PRIMARY KEY,name TEXT UNIQUE,score REAL);\n"
            "INSERT INTO t(name,score) VALUES ('alpha',1.5),('beta',2.25),"
            "('gamma',9.75);\n"
            "SELECT count(*) FROM t;\n"
        ),
        "expect": {"stdout_equals": ["3"], "exit": 0, "files": ["a.db"]},
    },
    {
        "id": "p2/disk-reopen",
        "abi": "preview2",
        "fixture": P2,
        "group": "p2disk",
        "preopen": ["/work"],
        "argv": ["-batch", "/work/a.db"],
        "stdin": (
            ".bail on\n"
            "SELECT count(*) FROM t;\n"
            "SELECT group_concat(name, ',') FROM "
            "(SELECT name FROM t ORDER BY name);\n"
        ),
        "expect": {"stdout_equals": ["3", "alpha,beta,gamma"], "exit": 0},
    },
    {
        "id": "p2/disk-read-by-p1",
        "abi": "preview2",
        "fixture": P1,
        "group": "p2disk",
        "preopen": ["/work"],
        "argv": ["-batch", "/work/a.db"],
        "stdin": ".bail on\nSELECT count(*) FROM t;\n",
        "expect": {"stdout_equals": ["3"], "exit": 0,
                   "note": "the same file written through WASI 0.2 is read "
                           "by the Preview1 build (format compatibility)"},
    },
    {
        "id": "p2/readonly-preopen-reads-file",
        "abi": "preview2",
        "fixture": P2,
        "group": "p2ro-read",
        "seed": {"q.sql": "SELECT 123;\n"},
        "preopen": ["/work:ro"],
        "argv": ["-batch", ":memory:"],
        "stdin": ".read /work/q.sql\n",
        "expect": {"stdout_equals": ["123"], "exit": 0,
                   "note": "a read-only preopen grants reading: the "
                           "guest opens a seeded SQL file through it"},
    },
    {
        "id": "p2/stderr-and-error-status",
        "abi": "preview2",
        "fixture": P2,
        "argv": ["-batch", ":memory:"],
        "stdin": "select * from nope;\n",
        "expect": {"stdout_equals": [], "exit_nonzero": True,
                   "stderr_contains": ["no such table"],
                   "note": "guest diagnostics go to stderr, never to "
                           "stdout; the shell's error is a non-zero exit"},
    },
    {
        "id": "p2/large-stdout-stream",
        "abi": "preview2",
        "fixture": P2,
        "argv": ["-batch", ":memory:"],
        "stdin": (
            ".bail on\n"
            "WITH RECURSIVE c(x) AS (VALUES(1) UNION ALL "
            "SELECT x+1 FROM c WHERE x<20000) SELECT x FROM c;\n"
        ),
        "expect": {"stdout_line_count": 20000, "exit": 0,
                   "stdout_contains": ["1\n", "20000\n"],
                   "note": "~89 KiB of guest output exercises chunked "
                           "stream writes, not a single small buffer"},
    },
    {
        "id": "p2/no-preopen-denied",
        "abi": "preview2",
        "fixture": P2,
        "argv": ["-batch", "HOSTPATH"],
        "stdin": ".bail on\nCREATE TABLE z(x);\n",
        "expect": {"exit_nonzero": True, "absent": ["HOSTPATH"]},
    },
    {
        "id": "p2/read-only-preopen-denies-write",
        "abi": "preview2",
        "fixture": P2,
        "group": "p2ro",
        "preopen": ["/work:ro"],
        "argv": ["-batch", "/work/x.db"],
        "stdin": ".bail on\nCREATE TABLE z(x);\n",
        "expect": {"exit_nonzero": True, "absent": ["x.db"]},
    },
    # ---------------- WASI 0.3.2 ----------------
    # Ten hand-authored @0.3.2 guests are pinned (tools/build-p3-fixtures.lisp,
    # P3-CHECKSUMS): a missing fixture is a FAILURE, never a skip, so the P3
    # ABI cannot look covered by absence. This first case is only the
    # no-import smoke; the provider cases below are the coverage.
    {
        "id": "p3/guest-smoke",
        "abi": "preview3",
        "fixture": P3,
        "argv": [],
        "stdin": "",
        # the pinned hand-authored command exports `run` returning ok and
        # writes nothing; regenerate with tools/build-p3-fixtures.lisp
        "expect": {"stdout_equals": [], "exit": 0},
    },
    {
        "id": "p3/import-exit",
        "abi": "preview3",
        "fixture": P3_IMPORT,
        "argv": [],
        "stdin": "",
        # imports wasi:cli/exit@0.3.2, lowers exit-with-code, calls it with
        # 7: the command runner maps the WASI exit to the process status.
        "expect": {"stdout_equals": [], "exit": 7},
    },
    {
        "id": "p3/cancel-subtask",
        "abi": "preview3",
        "fixture": P3_CANCEL,
        "argv": [],
        "stdin": "",
        # lowers the clock's async wait-for, cancels the pending subtask
        # synchronously (state must be cancelled) and drops it; a 1 s wait
        # finishing quickly proves the cancelling path ran.
        "expect": {"stdout_equals": [], "exit": 0},
    },
    {
        "id": "p3/stream-stdout",
        "abi": "preview3",
        "fixture": P3_STREAM,
        "argv": [],
        "stdin": "",
        # write-via-stream: canonical future order (park read, stage write,
        # drop writable, waitable-set.wait on the future event); stdout is
        # the guest's bytes.
        "expect": {"stdout_equals": ["hi"], "exit": 0},
    },
    {
        "id": "p3/async-callback",
        "abi": "preview3",
        "fixture": P3_ASYNC,
        "argv": [],
        "stdin": "",
        # async run lifted with a callback: the core run returns YIELD, the
        # callback resumes, calls task.return(ok) and returns EXIT.
        "expect": {"stdout_equals": [], "exit": 0},
    },
    {
        "id": "p3/clock-monotonic",
        "abi": "preview3",
        "fixture": P3_CLOCK,
        "argv": [],
        "stdin": "",
        # imports wasi:clocks/monotonic-clock@0.3.2, calls now() twice and
        # traps unless the second call is >= the first.
        "expect": {"stdout_equals": [], "exit": 0},
    },
    {
        "id": "p3/random-bytes",
        "abi": "preview3",
        "fixture": P3_RANDOM,
        "argv": [],
        "stdin": "",
        # imports wasi:random/random@0.3.2 (+ insecure and insecure-seed),
        # lowers the list-returning funcs with memory+realloc, and exits 0
        # only when two 16-byte reads have exact lengths and differ and the
        # u64/seed calls complete (any failure traps -> nonzero exit).
        "expect": {"stdout_equals": [], "exit": 0},
    },
    {
        "id": "p3/deadline-wait",
        "abi": "preview3",
        "fixture": P3_DEADLINE,
        "argv": [],
        "stdin": "",
        # lowers the clock's async wait-for (200 ms), joins the subtask into
        # a waitable set and returns WAIT; the CLI reactor's callback loop
        # parks until the deadline fires and the subtask resolves RETURNED.
        # The resumed callback traps unless the elapsed monotonic clock
        # time is >= the requested 200 ms, so an early completion fails.
        # The bound is deliberately past the async lift's old ~100 ms
        # quiescence budget: the drain oracle must treat the ARMED reactor
        # deadline as future progress, not as a deadlock.
        "expect": {"stdout_equals": [], "exit": 0},
    },
    {
        "id": "p3/cli-stdin-env-stderr-terminal",
        "abi": "preview3",
        "fixture": P3_CLI,
        # --env carries the environment probe pair, the expected argv[0]
        # (the fixture path exactly as the runner passed it) and the
        # lane's non-tty expectation for get-terminal-stdout.
        "env": ["WMD_ENV_PROBE=env-probe-value",
                "WMD_EXPECT_ARGV0={fixture}",
                "WMD_EXPECT_TERMINAL=none"],
        "argv": ["wmd-cli-marker"],
        # the marker the guest requires byte-for-byte through real stdin.
        "stdin": "stdin-probe\n",
        # imports wasi:cli/{stdin,environment,stderr,terminal-stdout}@0.3.2:
        # the guest traps unless argv[0] is the fixture path, argv[1] is the
        # marker, the environment holds WMD_ENV_PROBE=env-probe-value,
        # get-initial-cwd is an option and get-terminal-stdout is `none'
        # on the captured (non-tty) stdout. It reads stdin through
        # read-via-stream + stream.read (packed CopyResult|progress<<4,
        # a BLOCKED copy joined into the waitable set and waited), requires
        # exactly "stdin-probe\n" and the explicit DROPPED EOF result, and
        # requires the paired future to read back result::ok. A different
        # stdin byte traps (nonzero exit), so the exit status is the
        # assertion. It then writes the fixed message through stderr's
        # write-via-stream. stdout stays empty.
        "expect": {"stdout_equals": [], "exit": 0,
                   "stderr_contains": ["p3-cli-guest: stderr probe"]},
    },
    {
        "id": "p3/filesystem-preopens-read",
        "abi": "preview3",
        "fixture": P3_FS,
        # the preopen the guest looks up by its guest-visible name, and
        # the seeded file it opens THROUGH that preopen (relative path).
        "preopen": ["/work"],
        "seed": {"hello.txt": "filesystem-probe\n"},
        "argv": [],
        "stdin": "",
        # imports wasi:filesystem/{types,preopens}@0.3.2: get-directories
        # lists a "/work" preopen, open-at (async, waited as a SUBTASK
        # event) opens hello.txt, read-via-stream + stream.read copy the
        # seeded 17 bytes (asserted equal), the paired future resolves ok,
        # and a second open-at of a missing path must return ERR with
        # error-code::no-entry (case 19) — an err result, never a trap.
        # Any mismatch traps, so the exit status is the assertion.
        "expect": {"stdout_equals": [], "exit": 0},
    },
    # ---------------- Preview 1 ----------------
    {
        "id": "p1/select-memory",
        "abi": "preview1",
        "fixture": P1,
        "argv": ["-batch", ":memory:"],
        "stdin": ".bail on\nSELECT 6*7;\n",
        "expect": {"stdout_equals": ["42"], "exit": 0},
    },
    {
        "id": "p1/disk-persist-write",
        "abi": "preview1",
        "fixture": P1,
        "group": "p1disk",
        "preopen": ["/work"],
        "argv": ["-batch", "/work/p1.db"],
        "stdin": (
            ".bail on\n"
            "CREATE TABLE t(id INTEGER PRIMARY KEY,name TEXT);\n"
            "INSERT INTO t(name) VALUES ('alpha'),('beta'),('gamma');\n"
            "SELECT count(*) FROM t;\n"
        ),
        "expect": {"stdout_equals": ["3"], "exit": 0, "files": ["p1.db"]},
    },
    {
        "id": "p1/disk-persist-reopen",
        "abi": "preview1",
        "fixture": P1,
        "group": "p1disk",
        "preopen": ["/work"],
        "argv": ["-batch", "/work/p1.db"],
        "stdin": ".bail on\nSELECT count(*) FROM t;\n",
        "expect": {"stdout_equals": ["3"], "exit": 0},
    },
    {
        "id": "p1/no-preopen-denied",
        "abi": "preview1",
        "fixture": P1,
        "argv": ["-batch", "HOSTPATH"],
        "stdin": ".bail on\nCREATE TABLE z(x);\n",
        "expect": {"exit_nonzero": True, "absent": ["HOSTPATH"]},
    },
]

ABI_FILTERS = {"preview1": {"preview1"}, "preview2": {"preview2"},
               "preview3": {"preview3"},
               # "all" is the released-ABI gate (preview1 + preview2).
               # preview3 is a hand-authored ABI lane with no upstream
               # toolchain input, so it stays explicit (--abi preview3) and
               # `make test-wasi-preview3` runs it in both backends.
               "all": {"preview1", "preview2"}}


def runtime_version() -> str:
    try:
        out = subprocess.run(
            [os.environ.get("SBCL", "sbcl"), "--script",
             str(ROOT / "tools" / "run-wasi-guest.lisp"), "--version"],
            capture_output=True, text=True, timeout=180)
        return out.stdout.strip() or "unknown"
    except Exception:
        return "unknown"


def file_sha256(path: pathlib.Path) -> str:
    if not path.exists():
        return ""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def check(expect, out, err, rc, workdir):
    """Return (ok, detail). Every expectation must hold."""
    problems = []
    if "exit" in expect and rc != expect["exit"]:
        problems.append(f"exit {rc} != {expect['exit']}")
    if expect.get("exit_nonzero") and rc == 0:
        problems.append("exit 0 but a non-zero status was required")
    if "stdout_equals" in expect:
        want = ("\n".join(expect["stdout_equals"]) + "\n"
                if expect["stdout_equals"] else "")
        if out != want:
            problems.append(f"stdout {out!r} != {want!r}")
    if "stdout_line_count" in expect:
        got = len([ln for ln in out.splitlines() if ln != ""])
        if got != expect["stdout_line_count"]:
            problems.append(f"stdout has {got} lines, "
                            f"expected {expect['stdout_line_count']}")
    for needle in expect.get("stdout_contains", []):
        if needle not in out:
            problems.append(f"stdout lacks {needle!r} (got {out!r})")
    for needle in expect.get("stderr_contains", []):
        if needle not in err:
            problems.append(f"stderr lacks {needle!r}")
    for name in expect.get("files", []):
        p = workdir / name
        if not p.exists() or p.stat().st_size == 0:
            problems.append(f"expected file {name} missing or empty")
    for name in expect.get("absent", []):
        if (workdir / name).exists():
            problems.append(f"{name} must not exist")
    if problems:
        # stderr is diagnostics only; include a bounded sample on failure
        # (and name the compiled backend's heap exhaustion explicitly —
        # it is the one failure whose message otherwise hides in a
        # 60-line SBCL crash report)
        sample = err.strip().splitlines()[:3]
        note = ""
        if "Heap exhausted" in err:
            note = " [SBCL heap exhausted while compiling the core " \
                   "(raise --heap-mb)]"
        return False, "; ".join(problems) + (
            f" :: stderr={' | '.join(sample)}" if sample else "") + note
    return True, "ok"


def run_case(case, backend, groupdir, heap_mb):
    fixture = FIXTURES / case["fixture"]
    if not fixture.exists():
        return False, f"missing fixture {fixture} (failing prerequisite)"
    workdir = groupdir
    for name, content in case.get("seed", {}).items():
        path = workdir / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
    argv = [os.environ.get("SBCL", "sbcl"),
            "--dynamic-space-size", str(case.get("heap_mb", heap_mb)),
            "--control-stack-size", "16",
            "--script", str(ROOT / "tools" / "run-wasi-guest.lisp"),
            "run", str(fixture), "--wasi"]
    for spec in case.get("preopen", []):
        if spec.endswith(":ro"):
            argv += ["--dir", f"{workdir}::{spec[:-3]}:ro"]
        else:
            argv += ["--dir", f"{workdir}::{spec}"]
    # --env takes K=V before `--': the pairs reach wasi:cli/environment,
    # and `{fixture}' expands to the path the runner hands the guest as
    # argv[0], so a case can assert on that exact string.
    for spec in case.get("env", []):
        argv += ["--env", spec.replace("{fixture}", str(fixture))]
    guest_argv = []
    for a in case.get("argv", []):
        guest_argv.append(str(workdir / "host-denied.db")
                          if a == "HOSTPATH" else a)
    argv += ["--"] + guest_argv
    env = dict(os.environ)
    if backend == "compiled":
        env["WASMDOWN_API_MODE"] = "compiled"
    else:
        env.pop("WASMDOWN_API_MODE", None)
    env.setdefault("HOME", str(workdir))
    t0 = time.time()
    try:
        proc = subprocess.run(argv, input=case.get("stdin", ""),
                              capture_output=True, text=True, env=env,
                              timeout=case.get("timeout", 300))
        out, err, rc = proc.stdout, proc.stderr, proc.returncode
    except subprocess.TimeoutExpired:
        return False, f"timeout after {case.get('timeout', 300)}s"
    elapsed = time.time() - t0
    ok, detail = check(case["expect"], out, err, rc, workdir)
    if not ok:
        # keep the exact guest artifacts: a lane that reports a failure
        # without the bytes cannot be acted on
        fdir = pathlib.Path(FAILURE_DIR)
        fdir.mkdir(parents=True, exist_ok=True)
        stem = case["id"].replace("/", "-")
        (fdir / f"{stem}.stdout.txt").write_text(out)
        (fdir / f"{stem}.stderr.txt").write_text(err)
        (fdir / f"{stem}.exit").write_text(str(rc))
    return ok, detail, elapsed, out, rc


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--abi", choices=sorted(ABI_FILTERS), default="all")
    ap.add_argument("--backend", choices=["interpret", "compiled"],
                    default="interpret")
    ap.add_argument("--repeat", type=int, default=1)
    ap.add_argument("--output-dir", default=str(ROOT / "tests" /
                                                "wasi-results" / "latest"))
    ap.add_argument("--heap-mb", type=int, default=8192,
                    help="SBCL dynamic space for each guest process; the "
                         "compiled backend compiles a 6 MB SQLite core and "
                         "needs far more than the 1 GB default")
    ap.add_argument("--commit-summary", action="store_true",
                    help="also write docs/conformance/wasi.md. "
                         "Opt-in: a gate run must not clobber the committed "
                         "stability artifact, which is produced deliberately "
                         "by the documented final command")
    ap.add_argument("--list", action="store_true",
                    help="print the selected case ids and exit")
    args = ap.parse_args()
    if args.repeat < 1:
        sys.exit("--repeat must be a positive integer")
    ids = [c["id"] for c in CASES]
    dupes = sorted({i for i in ids if ids.count(i) > 1})
    if dupes:
        sys.exit(f"duplicate case ids in the manifest: {', '.join(dupes)}")
    selected = [c for c in CASES if c["abi"] in ABI_FILTERS[args.abi]]
    if not selected:
        # selecting an ABI with no cases is a configuration error, never
        # a silent "0/0 pass"
        sys.exit(f"no cases selected for --abi {args.abi}")
    if args.list:
        for c in selected:
            print(c["id"])
        return 0

    outdir = pathlib.Path(args.output_dir)
    if outdir.exists():
        shutil.rmtree(outdir)
    outdir.mkdir(parents=True)
    global FAILURE_DIR
    FAILURE_DIR = outdir / "failures"
    version = runtime_version()
    sbcl = subprocess.run([os.environ.get("SBCL", "sbcl"), "--version"],
                          capture_output=True, text=True).stderr.strip() \
        or subprocess.run([os.environ.get("SBCL", "sbcl"), "--version"],
                          capture_output=True, text=True).stdout.strip()

    results = []
    identity = source_identity()   # captured once: the tested tree, not the
                                   # tree at write time
    failures = 0
    for rep in range(1, args.repeat + 1):
        # one work directory per group per repetition, created fresh
        groups = {}

        def groupdir(name):
            if name not in groups:
                safe = name.replace("/", "-").replace(" ", "_")
                d = pathlib.Path(tempfile.mkdtemp(prefix=f"wmd-wasi-{safe}-"))
                groups[name] = d
            return groups[name]

        for case in selected:
            workdir = groupdir(case.get("group", "solo-" + case["id"]))
            outcome = run_case(case, args.backend, workdir, args.heap_mb)
            if len(outcome) == 2:
                ok, detail = outcome
                elapsed, out, rc = 0.0, "", None
            else:
                ok, detail, elapsed, out, rc = outcome
            if not ok:
                failures += 1
            results.append({
                **identity,
                "schema": "wasmdown.test-result.v1",
                "suite": "wasi-" + case["abi"],
                "case_id": case["id"],
                "abi": case["abi"],
                "fixture": case["fixture"],
                "fixture_sha256": file_sha256(FIXTURES / case["fixture"]),
                "backend": args.backend,
                "runtime": version,
                "sbcl": sbcl,
                "run": rep,
                "outcome": "pass" if ok else "fail",
                "detail": detail,
                "exit": rc,
                "elapsed_s": round(elapsed, 3),
            })
        for d in groups.values():
            shutil.rmtree(d, ignore_errors=True)

    jsonl = outdir / "results.jsonl"
    with open(jsonl, "w") as f:
        for r in results:
            f.write(json.dumps(r) + "\n")

    # ---- summary ----
    total = len(results)
    passes = sum(1 for r in results if r["outcome"] == "pass")
    by_abi = {}
    for r in results:
        d = by_abi.setdefault(r["abi"], {"pass": 0, "fail": 0})
        d[r["outcome"]] += 1
    md = [f"# WASI conformance lane ({args.abi}, {args.backend})", ""]
    md.append("Generated by `python3 tools/run-wasi-suite.py --abi all "
              "--commit-summary` (the numbers also come from `make test-wasi`) "
              "— a **gate**, not a baseline. Lane index: "
              "[`docs/conformance/README.md`](README.md).")
    md.append("")
    md.append(f"- runtime: `{version}` on `{sbcl}`")
    md.append(f"- repetitions: {args.repeat}; cases per run: "
              f"{len(selected)}; total results: {total}")
    md.append(f"- result records: `{jsonl}` (wasmdown.test-result.v1)")
    md.append("")
    planned = {}
    for c in selected:
        planned[c["abi"]] = planned.get(c["abi"], 0) + 1
    md += ["| ABI | planned | executed | pass | fail |",
           "| --- | --- | --- | --- | --- |"]
    for abi, d in sorted(by_abi.items()):
        md.append(f"| {abi} | {planned[abi] * args.repeat} | "
                  f"{d['pass'] + d['fail']} | {d['pass']} | {d['fail']} |")
    md.append("")
    md.append(f"**{passes}/{total} pass**")
    md.append("")
    bad = [r for r in results if r["outcome"] != "pass"]
    if bad:
        md += ["## Failures", "", "| case | run | detail |",
               "| --- | --- | --- |"]
        for r in bad:
            md.append(f"| {r['case_id']} | {r['run']} | "
                      f"{r['detail'].replace('|', '\\|')} |")
    summary = outdir / "summary.md"
    summary.write_text("\n".join(md) + "\n")
    # the committed summary is written only when asked (see
    # --commit-summary); the JSONL stays under tests/, gitignored
    if args.commit_summary:
        target = ROOT / "docs" / "conformance" / "wasi.md"
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("\n".join(md) + "\n")
        print(f"committed: {target}")

    print(f"{passes}/{total} pass  ({args.abi}/{args.backend}, "
          f"{args.repeat} run(s))")
    if bad:
        for r in bad:
            print(f"  FAIL {r['case_id']} (run {r['run']}): {r['detail']}")
        return 1
    print(f"summary: {summary}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
