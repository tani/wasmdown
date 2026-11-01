#!/usr/bin/env python3
"""Upstream WebAssembly/testsuite adapter (docs/design/PLAN.md §12.7).

Fetches the aggregated testsuite mirror at an EXACT commit, runs it
through the WAST runner (tools/run-upstream-wast.lisp) and writes:

    tests/upstream-results.jsonl     one result per command
    tests/upstream-testsuite/.pin.json  the fetched commit + SHA-256

This is a DEVELOPMENT/DISCOVERY lane, never a release gate:
  * the mirror is not vendored (specs/lock.toml marks it
    "not-vendored");
  * proposal corpora (GC, EH, threads, ...) may classify as
    `unsupported` and must not be counted twice against the pinned
    core suite;
  * failures are quantified honestly, in the summary, not converted.

Usage:
    python3 tools/run-upstream-testsuite.py [--commit SHA] [--keep]
    --commit defaults to the pinned mirror commit recorded in
    specs/lock.toml under [spec.testsuite-aggregate].
"""

import argparse
import hashlib
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parent.parent
LOCK = ROOT / "specs" / "lock.toml"
DEST = ROOT / "tests" / "upstream-testsuite"
JSONL = ROOT / "tests" / "upstream-results.jsonl"


def pinned_commit() -> str:
    """The mirror commit in specs/lock.toml, else the vendored core commit."""
    text = LOCK.read_text()
    for block in text.split("[")[1:]:
        if block.startswith("spec.testsuite-aggregate"):
            m = re.search(r'commit\s*=\s*"([0-9a-f]{40})"', block)
            if m:
                return m.group(1)
    m = re.search(r'commit\s*=\s*"([0-9a-f]{40})"', text)
    if not m:
        sys.exit("no pinned commit in specs/lock.toml")
    return m.group(1)


def fetch(commit: str) -> pathlib.Path:
    url = ("https://codeload.github.com/WebAssembly/testsuite/tar.gz/"
           + commit)
    tmp = pathlib.Path(tempfile.mkdtemp(prefix="wmd-upstream-"))
    tar_path = tmp / "testsuite.tar.gz"
    print(f"fetching {url}")
    with urllib.request.urlopen(url, timeout=120) as resp, \
            open(tar_path, "wb") as out:
        shutil.copyfileobj(resp, out)
    digest = hashlib.sha256(tar_path.read_bytes()).hexdigest()
    with tarfile.open(tar_path) as tar:
        tar.extractall(tmp, filter="data")
    roots = [p for p in tmp.iterdir() if p.is_dir()]
    if len(roots) != 1:
        sys.exit(f"unexpected archive layout: {roots}")
    return roots[0], digest


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--commit", default=None,
                    help="exact mirror commit (default: the lock pin)")
    ap.add_argument("--keep", action="store_true",
                    help="keep the fetched tree under tests/upstream-testsuite")
    args = ap.parse_args()
    commit = args.commit or pinned_commit()

    tree, digest = fetch(commit)
    files = sorted(tree.glob("*.wast"))
    if not files:
        sys.exit("no .wast files at the mirror root")

    run_dir = DEST if args.keep else pathlib.Path(tempfile.mkdtemp(
        prefix="wmd-upstream-wast-"))
    if args.keep and DEST.exists():
        shutil.rmtree(DEST)
    run_dir.mkdir(parents=True, exist_ok=True)
    for f in files:
        shutil.copy2(f, run_dir / f.name)
    (run_dir / ".pin.json").write_text(json.dumps(
        {"repo": "https://github.com/WebAssembly/testsuite",
         "commit": commit,
         "tarball_sha256": digest,
         "files": len(files)}, indent=2) + "\n")

    env = dict(os.environ,
               WASMDOWN_WAST_PATTERN=str(run_dir / "*.wast"),
               WASMDOWN_WAST_JSONL=str(JSONL),
               WASMDOWN_WAST_LABEL=f"upstream@{commit[:12]}")
    print(f"running {len(files)} scripts from {run_dir}")
    subprocess.run([os.environ.get("SBCL", "sbcl"),
                    "--control-stack-size", "16",
                    "--dynamic-space-size", "4096",
                    "--script", str(ROOT / "tools" / "run-upstream-wast.lisp")],
                   check=False, env=env)
    if not args.keep:
        shutil.rmtree(run_dir, ignore_errors=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
