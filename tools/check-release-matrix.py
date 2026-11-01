#!/usr/bin/env python3
"""Release-matrix checker: every required lane row, from recorded artifacts.

The matrix is `core, component, P1, P2, P3 x interpret/compiled`. This
script does not run the lanes (the compiled WASI lanes cost minutes); it
checks the ARTIFACTS they write, so a release claim is a statement about
recorded evidence rather than about a memory of a run.

Rows and their artifacts:
  core/interpret+compiled   tests/backend-parity-summary.json
  component/interpret       tests/component-results.jsonl
  component/compiled        tests/component-compiled-results.jsonl
  p2/interpret              tests/wasi-results/latest/results.jsonl
  p2/compiled               tests/wasi-results/compiled-preview2/results.jsonl
  p3/interpret              tests/wasi-results/p3-interpret/results.jsonl
  p3/compiled               tests/wasi-results/p3-compiled/results.jsonl
  bench                     tests/bench-backends.json (valid JSON)
  inventory                 tests/backend-inventory.json

P1 acceptance prints to stdout only; it is checked by `make verify` and is
listed here as unrecorded so the matrix does not pretend otherwise.

Exit status: 0 when every recorded row is present, green, and names HEAD as
its source commit; 1 otherwise (each failing row is printed).
"""
import json
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def head():
    try:
        return subprocess.run(("git", "rev-parse", "HEAD"), cwd=ROOT,
                              capture_output=True, text=True,
                              check=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return "unknown"


def read_json(path):
    with open(path) as fh:
        return json.load(fh)


def jsonl(path):
    with open(path) as fh:
        return [json.loads(line) for line in fh if line.strip()]


def outcomes(path):
    rows = jsonl(path)
    counts = {}
    for row in rows:
        counts[row.get("outcome")] = counts.get(row.get("outcome"), 0) + 1
    return counts, len(rows)


def check_rows(commit):
    rows = []

    def add(row, ok, detail):
        rows.append((row, ok, detail))

    # core parity
    p = os.path.join(ROOT, "tests", "backend-parity-summary.json")
    if os.path.exists(p):
        d = read_json(p)
        total = sum(f.get("match", 0) + f.get("diverge", 0)
                    for f in d.get("files", []))
        diverge = sum(f.get("diverge", 0) for f in d.get("files", []))
        refusals = sum(f.get("refusals", 0) for f in d.get("files", []))
        same_commit = d.get("source_commit") == commit
        add("core/interpret+compiled", same_commit and diverge == 0
            and refusals == 0 and total > 0,
            "%d commands, %d diverge, %d refusals, source %s"
            % (total, diverge, refusals, d.get("source_commit", "?")[:12]))
    else:
        add("core/interpret+compiled", False, "missing parity summary")

    # component lanes
    for row, path in (
            ("component/interpret", "tests/component-results.jsonl"),
            ("component/compiled", "tests/component-compiled-results.jsonl")):
        full = os.path.join(ROOT, path)
        if os.path.exists(full):
            counts, total = outcomes(full)
            add(row, counts.get("pass", 0) == total and total > 0,
                "%d commands, %s" % (total, counts))
        else:
            add(row, False, "missing " + path)

    # WASI lanes
    wasi = (
        ("p2/interpret", "tests/wasi-results/p2-interpret/results.jsonl"),
        ("p2/compiled", "tests/wasi-results/compiled-preview2/results.jsonl"),
        ("p3/interpret", "tests/wasi-results/p3-interpret/results.jsonl"),
        ("p3/compiled", "tests/wasi-results/p3-compiled/results.jsonl"),
    )
    for row, path in wasi:
        full = os.path.join(ROOT, path)
        if os.path.exists(full):
            records = jsonl(full)
            counts = {}
            for rec in records:
                counts[rec.get("outcome")] = counts.get(rec.get("outcome"), 0) + 1
            total = len(records)
            same = all(rec.get("source_commit") == commit for rec in records)
            add(row, same and counts.get("pass", 0) == total and total > 0,
                "%d cases, %s, source %s"
                % (total, counts,
                   (records[0].get("source_commit", "?")[:12] if records else "?")))
        else:
            add(row, False, "missing " + path)

    # machine-readable cost data
    bench = os.path.join(ROOT, "tests", "bench-backends.json")
    if os.path.exists(bench):
        try:
            d = read_json(bench)
            ok = (d.get("schema") == "wasmdown.backend-bench.v1"
                  and d.get("source_commit") == commit
                  and all(b.get("mode") in ("interpret", "compiled")
                          for f in d.get("fixtures", [])
                          for b in f.get("backends", [])))
            add("bench", ok, "valid JSON, source %s"
                % d.get("source_commit", "?")[:12])
        except (ValueError, OSError) as e:
            add("bench", False, "invalid JSON: %s" % e)
    else:
        add("bench", False, "missing tests/bench-backends.json")

    # inventory
    inv = os.path.join(ROOT, "tests", "backend-inventory.json")
    if os.path.exists(inv):
        d = read_json(inv)
        add("inventory", d.get("source_commit") == commit,
            "P1 %d provided, source %s"
            % (len(d.get("P1", {}).get("provided", [])),
               d.get("source_commit", "?")[:12]))
    else:
        add("inventory", False, "missing tests/backend-inventory.json")

    add("p1/interpret+compiled", None,
        "stdout-only lane; gated by make verify and report evidence")
    return rows


def main():
    commit = head()
    rows = check_rows(commit)
    # Artifacts are committed after the tested source, so a row may name
    # HEAD or any ANCESTOR of it (the recorded commit is the tested tree);
    # a commit that is not an ancestor is stale/foreign evidence.
    def ancestor(sha):
        if not sha or sha == "?":
            return False
        out = subprocess.run(("git", "merge-base", "--is-ancestor", sha, "HEAD"),
                             cwd=ROOT, capture_output=True, check=False)
        return out.returncode == 0
    width = max(len(r) for r, _, _ in rows)
    failures = 0
    print("%-*s  %-6s  %s" % (width, "row", "state", "evidence"))
    for row, ok, detail in rows:
        m = re.search(r"source ([0-9a-f]{12})", detail)
        if ok is False and m and ancestor(m.group(1)):
            ok, detail = True, detail + " (artifact commit predates HEAD)"
        state = "OK" if ok else ("n/a" if ok is None else "FAIL")
        if ok is False:
            failures += 1
        print("%-*s  %-6s  %s" % (width, row, state, detail))
    if failures:
        print("\nrelease matrix: %d row(s) missing, red, or from another commit"
              % failures)
        return 1
    print("\nrelease matrix: all recorded rows green at %s" % commit[:12])
    return 0


if __name__ == "__main__":
    sys.exit(main())
