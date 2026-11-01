#!/usr/bin/env python3
"""Upstream WASI testsuite lane (docs/design/PLAN.md §9.3 F1, §12.5).

Runs the precompiled guests of WebAssembly/wasi-testsuite through the real
WASMDOWN CLI via `adapters/wasmdown.py`, and records the outcome:

    tests/wasi-upstream-expectations.toml   generated declared-exclusion skips
    tests/wasi-upstream-results.json        deterministic counts + failures
    tests/wasi-upstream-summary.md          the human-readable summary

The checkout is NOT vendored. The driver:
  1. resolves the tree (--dir, WASI_TESTSuite_DIR/WASI_TESTSUITE_DIR, or
     third_party/wasi-testsuite),
  2. verifies `git rev-parse HEAD` equals the [suite.wasi-testsuite] commit
     pinned in specs/lock.toml,
  3. generates the expectations file from the tree itself: a test is skipped
     ONLY by one of the declared exclusions below, never to make a run green,
  4. invokes upstream `run-tests --runtime-adapter adapters/wasmdown.py`,
  5. post-processes the JSON, asserts the actual skips are exactly the
     planned ones, and writes the summary.

Exit codes:
  0   every admitted test passed (lane green)
  1   the lane ran and reported failures (the recorded blocker), artifacts written
  2   checkout absent / wrong commit / skip plan not reproducible
"""

import argparse
import importlib.util
import json
import os
import re
import subprocess
import sys
import tempfile
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
LOCK = ROOT / "specs" / "lock.toml"
ADAPTER = ROOT / "adapters" / "wasmdown.py"
EXPECTATIONS = ROOT / "tests" / "wasi-upstream-expectations.toml"
RESULTS = ROOT / "tests" / "wasi-upstream-results.json"
SUMMARY = ROOT / "tests" / "wasi-upstream-summary.md"
DEFAULT_DIR = ROOT / "third_party" / "wasi-testsuite"
PIN_SECTION = "suite.wasi-testsuite"

# The only reasons a test may be dropped from the lane. Keyed by category so
# the summary can count each exclusion separately.
DECLARED_EXCLUSIONS = {
    "proposal-http":
        "wasi:http is unsupported by design (permanent exclusion, "
        "2026-11-02; PLAN.md M16 slice 5 / M17) - not an implementation gap",
    "proposal-sockets":
        "network communication (wasi:sockets TCP/UDP/DNS) is outside the "
        "declared support profile (permanent exclusion, 2026-11-02)",
    "legacy-network-imports":
        "network communication is outside the declared support profile; the "
        "four legacy wasi_snapshot_preview1 sock_* imports are excluded "
        "(2026-11-02)",
}

LEGACY_NETWORK_PREFIX = "sock_"
MIXED_RELEASE = "mixes WASI release lines"
IMPORT_RE = re.compile(r"wasi:[A-Za-z0-9_./-]+@[0-9A-Za-z.+-]+")


# ---------------------------------------------------------------- pin / tree

def abort(message):
    """A lane-level blocker: nothing was measured."""
    print(message, file=sys.stderr)
    sys.exit(2)


def pinned_suite():
    with LOCK.open("rb") as fh:
        data = tomllib.load(fh)
    pin = data.get("suite", {}).get("wasi-testsuite")
    if not pin:
        abort(f"specs/lock.toml has no [{PIN_SECTION}] section")
    commit = pin.get("commit", "")
    if not re.fullmatch(r"[0-9a-f]{40}", commit):
        abort(f"specs/lock.toml [{PIN_SECTION}] commit is not a "
              f"40-hex SHA: {commit!r}")
    return pin


def checkout_dir(cli_dir):
    if cli_dir:
        return Path(cli_dir).expanduser().resolve()
    env_dir = (os.environ.get("WASI_TESTSuite_DIR")
               or os.environ.get("WASI_TESTSUITE_DIR"))
    if env_dir:
        return Path(env_dir).expanduser().resolve()
    return DEFAULT_DIR


def checkout_blocker(path, pin, detail):
    return f"""wasi-testsuite checkout unusable: {path}
{detail}

The lane needs the upstream precompiled guest tree at the pinned commit
(select it with WASI_TESTSuite_DIR=<path> or --dir <path>):

  git clone --branch {pin.get('branch', 'prod/testsuite-base')} \\
      {pin['repo']} {path}
  git -C {path} checkout {pin['commit']}
  python3 -m pip install colorama requests    # runner dependencies

Then: WASI_TESTSuite_DIR={path} make test-wasi-upstream

Recorded blocker: no local cargo/rustc/wasm-tools toolchain emits @0.3.2 in
this environment, so only the upstream precompiled binaries can be measured;
their mixed WASI release lines (a LINK error by design) are the reason the
0.3 lane is not green. See docs/design/PLAN.md §9.3 F1."""


def verify_checkout(path, pin):
    if not path.is_dir():
        abort(checkout_blocker(path, pin, "directory does not exist"))
    for required in ("run-tests", "tests", "test-runner"):
        if not (path / required).exists():
            abort(checkout_blocker(
                path, pin, f"missing upstream entry point: {required}"))
    try:
        head = subprocess.run(
            ["git", "-C", str(path), "rev-parse", "HEAD"],
            capture_output=True, text=True, check=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError) as exc:
        abort(checkout_blocker(
            path, pin, f"cannot read git HEAD ({exc}); a pinned git checkout "
                       "is required"))
    if head != pin["commit"]:
        abort(checkout_blocker(
            path, pin,
            f"HEAD is {head}, the pin is {pin['commit']}.\n"
            f"Refusing to run a floating tree."))


# ------------------------------------------------------------ suite planning

def manifest_suites(suite_dir):
    suites = []
    for manifest in sorted((suite_dir / "tests").glob("*/testsuite/*/manifest.json")):
        data = json.loads(manifest.read_text(encoding="utf-8"))
        suites.append((manifest.parent, data["name"], data["version"]))
    if not suites:
        abort(f"no tests/<lang>/testsuite/<abi>/manifest.json under {suite_dir}")
    return suites


def test_config(test_dir, test_name):
    path = test_dir / f"{test_name}.json"
    if not path.exists():
        return {}
    return json.loads(path.read_text(encoding="utf-8"))


def exclusion_for(version, test_name, config):
    """The declared category that excludes this test, or None."""
    proposals = config.get("proposals") or []
    unknown = set(proposals) - {"http", "sockets"}
    if unknown:
        abort(
            f"{test_name} requests undeclared proposal(s) {sorted(unknown)}; "
            "add a declared exclusion with a rationale before measuring")
    if "http" in proposals:
        return "proposal-http"
    if "sockets" in proposals:
        return "proposal-sockets"
    if version == "wasm32-wasip1" and test_name.startswith(LEGACY_NETWORK_PREFIX):
        return "legacy-network-imports"
    world = config.get("world", "wasi:cli/command")
    if world != "wasi:cli/command":
        abort(
            f"{test_name} targets world {world} without a declared exclusion; "
            "the adapter claims wasi:cli/command only")
    return None


def plan_exclusions(suites):
    """suite name -> {test: category} for the declared exclusions only."""
    plan = {}
    for test_dir, name, version in suites:
        entries = {}
        for wasm in sorted(test_dir.glob("*.wasm")):
            test_name = wasm.stem
            category = exclusion_for(
                version, test_name, test_config(test_dir, test_name))
            if category:
                entries[test_name] = category
        plan[name] = entries
    return plan


def toml_string(value):
    return json.dumps(value, ensure_ascii=False)


def write_expectations(path, plan):
    lines = [
        "# Generated by tools/run-wasi-testsuite.py from the pinned",
        "# WebAssembly/wasi-testsuite tree. Do not edit by hand.",
        "#",
        "# Every entry is a DECLARED exclusion, never a failing test:",
    ]
    for category, reason in DECLARED_EXCLUSIONS.items():
        lines.append(f"#   {category}: {reason}")
    lines += ["", "version = 1", ""]
    for suite_name in sorted(plan):
        lines.append("[[suite]]")
        lines.append(f"name = {toml_string(suite_name)}")
        for test_name in sorted(plan[suite_name]):
            category = plan[suite_name][test_name]
            lines.append("")
            lines.append(f"# {category}")
            lines.append("[[suite.test]]")
            lines.append(f"name = {toml_string(test_name)}")
            lines.append('action = "skip"')
        lines.append("")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines).rstrip() + "\n", encoding="utf-8")


# --------------------------------------------------------------------- run

def run_upstream(suite_dir, expectations, raw_json):
    command = [
        sys.executable, str(suite_dir / "run-tests"),
        "--runtime-adapter", str(ADAPTER),
        "--expectations", str(expectations),
        "--json-output-location", str(raw_json),
        "--disable-colors",
    ]
    env = dict(os.environ)
    env["WASMDOWN_ROOT"] = str(ROOT)
    proc = subprocess.run(command, cwd=str(suite_dir), capture_output=True,
                          text=True, env=env)
    return command, proc


# --------------------------------------------------------------- postprocess

def classify(message):
    if MIXED_RELEASE in message:
        return "link-mixed-release-lines"
    if "link error" in message:
        return "link-error"
    if "Assertion failed" in message:
        return "guest-assertion"
    if "expected" in message.lower():
        return "expectation-mismatch"
    return "other"


def release_lines(messages):
    versions = set()
    for message in messages:
        versions.update(m.split("@", 1)[1] for m in IMPORT_RE.findall(message))
    return sorted(versions)


def filter_suites(raw, plan):
    """Keep the four manifest suites; reject tests from anywhere else.

    `run-tests` walks every directory under `tests/`, and the Rust sources
    ship an extra `src/bin/manifest.json` (a build-input manifest, 0 tests).
    Anything with tests outside the planned suites is a tree layout change
    the driver must not silently absorb.
    """
    kept, dropped = [], []
    for suite in raw["results"]:
        if suite["name"] in plan:
            kept.append(suite)
        elif suite["tests"]:
            abort(f"upstream tree has an unplanned suite with tests: "
                     f"{suite['name']} ({len(suite['tests'])} tests)")
        else:
            dropped.append(suite["name"])
    raw["results"] = kept
    return dropped


def verify_skips(raw, plan, suites):
    actual = set()
    for suite in raw["results"]:
        for test in suite["tests"]:
            if test["outcome"] == "skip":
                actual.add((suite["name"], test["name"]))
    expected = {(suite, test)
                for suite, entries in plan.items()
                for test in entries}
    missing = sorted(expected - actual)
    unexpected = sorted(actual - expected)
    if missing or unexpected:
        abort(
            "skip plan is not reproducible at this commit:\n"
            f"  planned but not skipped: {missing}\n"
            f"  skipped but not declared: {unexpected}")
    observed = sum(len(suite["tests"]) for suite in raw["results"])
    on_disk = sum(len(list(test_dir.glob("*.wasm")))
                  for test_dir, _, _ in suites)
    if observed != on_disk:
        abort(f"test denominator drift: runner reported {observed} tests, "
                 f"the pinned tree holds {on_disk} *.wasm under the "
                 "planned suites")


def adapter_meta():
    """The adapter's own declared contract, so the artifact cannot drift."""
    spec = importlib.util.spec_from_file_location("wasmdown_wasi_adapter", ADAPTER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return {
        "path": "adapters/wasmdown.py",
        "name": module.get_name(),
        "version": module.get_version(),
        "wasi_versions": list(module.get_wasi_versions()),
        "wasi_worlds": list(module.get_wasi_worlds()),
        "timeout_seconds": module.get_timeout_seconds(),
    }


def build_results(pin, plan, raw):
    suites = []
    for suite in sorted(raw["results"], key=lambda s: s["name"]):
        tests = []
        for test in sorted(suite["tests"], key=lambda t: t["name"]):
            entry = {
                "name": test["name"],
                "outcome": test["outcome"],
                "executed": test["executed"],
            }
            messages = [f for f in test["failures"] if f]
            if messages:
                entry["failures"] = messages
                entry["failure_kind"] = sorted({classify(m) for m in messages})
            tests.append(entry)
        suites.append({
            "name": suite["name"],
            "runtime": suite["runtime"],
            "passed": suite["passed"],
            "failed": suite["failed"],
            "skipped": suite["skipped"],
            "xfailed": suite["xfailed"],
            "xpassed": suite["xpassed"],
            "tests": tests,
        })

    totals = {key: sum(s[key] for s in suites)
              for key in ("passed", "failed", "skipped", "xfailed", "xpassed")}
    skips = []
    for suite_name in sorted(plan):
        for test_name in sorted(plan[suite_name]):
            category = plan[suite_name][test_name]
            skips.append({
                "suite": suite_name,
                "test": test_name,
                "category": category,
                "reason": DECLARED_EXCLUSIONS[category],
            })
    by_category = {}
    for skip in skips:
        by_category[skip["category"]] = by_category.get(skip["category"], 0) + 1

    failures = []
    for suite in suites:
        for test in suite["tests"]:
            if test["outcome"] not in ("fail", "xpass"):
                continue
            failures.append({
                "suite": suite["name"],
                "test": test["name"],
                "kinds": test.get("failure_kind", ["other"]),
                "messages": test.get("failures", []),
            })
    kinds = {}
    for failure in failures:
        for kind in failure["kinds"]:
            kinds[kind] = kinds.get(kind, 0) + 1

    return {
        "schema": "wasmdown.wasi-upstream.v1",
        "upstream": {
            "repo": pin["repo"],
            "branch": pin.get("branch"),
            "commit": pin["commit"],
        },
        "adapter": adapter_meta(),
        "totals": totals,
        "suites": suites,
        "skips": {
            "by_category": dict(sorted(by_category.items())),
            "declared_reasons": dict(sorted(DECLARED_EXCLUSIONS.items())),
            "entries": skips,
        },
        "failures": {
            "by_kind": dict(sorted(kinds.items())),
            "entries": failures,
        },
    }


def write_results(path, results):
    path.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n",
                    encoding="utf-8")


def counts_line(entry):
    return (f"{entry['passed']} pass / {entry['failed']} fail / "
            f"{entry['skipped']} skip / {entry['xfailed']} xfail / "
            f"{entry['xpassed']} xpass")


def write_summary(path, results, command, suite_dir):
    totals = results["totals"]
    # the runner's JSON lands in a temp dir; keep the command reproducible
    command = [re.sub(r"/tmp/wasi-upstream-[^/]+/results\.json",
                      "<tmp>/results.json", part) for part in command]
    lines = [
        "# Upstream wasi-testsuite lane (PLAN.md §9.3 F1)",
        "",
        "Generated by `tools/run-wasi-testsuite.py` (`make test-wasi-upstream`).",
        "The guest tree is not vendored; this page is the recorded evidence of",
        "one run at the pinned upstream commit.",
        "",
        "## Pin",
        "",
        f"- repo: `{results['upstream']['repo']}`",
        f"- branch: `{results['upstream']['branch']}`",
        f"- commit: `{results['upstream']['commit']}`",
        f"- checkout: `{suite_dir}`",
        "",
        "## Command",
        "",
        "```",
        " ".join(command),
        "```",
        "",
        "## Counts",
        "",
        "| Suite | Result |",
        "| --- | --- |",
    ]
    for suite in results["suites"]:
        lines.append(f"| {suite['name']} | {counts_line(suite)} |")
    lines += [
        "",
        f"Total: **{totals['passed']} pass / {totals['failed']} fail / "
        f"{totals['skipped']} skip / {totals['xfailed']} xfail / "
        f"{totals['xpassed']} xpass** over "
        f"{sum(totals.values())} pinned tests.",
        "",
        "## Skips (declared exclusions only)",
        "",
        "| Category | Tests | Rationale |",
        "| --- | --- | --- |",
    ]
    for category, count in results["skips"]["by_category"].items():
        lines.append(
            f"| `{category}` | {count} | {DECLARED_EXCLUSIONS[category]} |")
    lines += [
        "",
        "The driver regenerates this list from the tree and fails if the actual",
        "skips differ from it; no failing test is skipped.",
        "",
        "## Failures",
        "",
    ]
    if results["failures"]["by_kind"]:
        lines += ["| Kind | Tests |", "| --- | --- |"]
        for kind, count in results["failures"]["by_kind"].items():
            lines.append(f"| `{kind}` | {count} |")
        lines.append("")
    else:
        lines += ["None.", ""]

    link_failures = [f for f in results["failures"]["entries"]
                     if "link-mixed-release-lines" in f["kinds"]]
    if link_failures:
        messages = [m for f in link_failures for m in f["messages"]]
        versions = release_lines(messages)
        lines += [
            "## Recorded blocker: mixed WASI release lines",
            "",
            f"{len(link_failures)} of {totals['failed']} failing tests are "
            "refused by the linker because the upstream precompiled @0.3.2 "
            "guests import interfaces from more than one WASI release line "
            "(the runtime refuses a mixed provider graph by design).",
            "",
            "Release lines observed in the link errors: "
            + ", ".join(f"`{v}`" for v in versions) + ".",
            "",
            "Representative message:",
            "",
            "```",
            messages[0],
            "```",
            "",
        ]

    signatures = {}
    for failure in results["failures"]["entries"]:
        text = re.sub(r"\s+", " ", " ".join(failure["messages"]))
        text = re.sub(r"\d+", "#", text)[:160]
        signatures.setdefault(text, []).append(
            f"{failure['suite']}: {failure['test']}")
    if signatures:
        lines += [
            "## Failure signatures",
            "",
            "Identical diagnostics grouped (digits masked); a repeated",
            "signature is one root cause, not N defects.",
            "",
            "| Tests | Signature |",
            "| --- | --- |",
        ]
        for text, tests in sorted(signatures.items(),
                                  key=lambda kv: (-len(kv[1]), kv[0])):
            cell = text.replace("|", "\\|").replace("`", "'")
            lines.append(f"| {len(tests)} | `{cell}` |")
        lines.append("")
    lines += [
        "## Notes",
        "",
        "- Every guest is one fresh `sbcl --script tools/wasmdown.lisp run` "
        "process; no state crosses tests.",
        "- A wasm32-wasip1 test is admitted only when the runtime's declared "
        "(non-network) P1 surface covers it.",
        "- Durations and timestamps are deliberately omitted so the recorded "
        "counts are reproducible.",
        "",
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


# -------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dir", default=None,
                    help="wasi-testsuite checkout (default: "
                         "WASI_TESTSuite_DIR, else third_party/wasi-testsuite)")
    ap.add_argument("--results", default=str(RESULTS),
                    help="deterministic JSON output")
    ap.add_argument("--summary", default=str(SUMMARY),
                    help="Markdown summary output")
    ap.add_argument("--expectations", default=str(EXPECTATIONS),
                    help="generated expectations TOML")
    args = ap.parse_args()

    pin = pinned_suite()
    suite_dir = checkout_dir(args.dir)
    verify_checkout(suite_dir, pin)

    suites = manifest_suites(suite_dir)
    plan = plan_exclusions(suites)
    expectations = Path(args.expectations)
    write_expectations(expectations, plan)

    with tempfile.TemporaryDirectory(prefix="wasi-upstream-") as tmp:
        raw_json = Path(tmp) / "results.json"
        command, proc = run_upstream(suite_dir, expectations, raw_json)
        sys.stdout.write(proc.stdout)
        if proc.stderr:
            sys.stderr.write(proc.stderr)
        if not raw_json.exists():
            abort("upstream runner produced no JSON output "
                     f"(exit {proc.returncode}); see stderr above")
        raw = json.loads(raw_json.read_text(encoding="utf-8"))

    filter_suites(raw, plan)
    verify_skips(raw, plan, suites)
    results = build_results(pin, plan, raw)
    write_results(Path(args.results), results)
    write_summary(Path(args.summary), results, command, suite_dir)

    totals = results["totals"]
    print(f"wasi upstream: {totals['passed']} pass / {totals['failed']} fail / "
          f"{totals['skipped']} skip "
          f"({results['upstream']['commit'][:12]})")
    print(f"artifacts: {args.results} , {args.summary}")
    if totals["failed"]:
        kinds = ", ".join(f"{k}={v}"
                          for k, v in results["failures"]["by_kind"].items())
        print(f"recorded blocker: {kinds}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
