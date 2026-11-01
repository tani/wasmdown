#!/usr/bin/env python3
"""Backend / ABI inventory for wasmdown.

Evidence comes from two places ONLY:

  * the pinned specification trees (``specs/wasi/preview1/INVENTORY.md``,
    the vendored ``specs/wasi/wasi-wit-<release>/`` packages, and
    ``specs/lock.toml`` for the pins), and
  * the runtime's REAL registration: ``tools/backend-inventory.lisp``
    loads the system in SBCL and prints the ``*p1-signatures*`` /
    ``*p1-implementations*`` tables, the ``p2-builder-table`` keys for
    every vendored release, the ``%wasi-provider-builders`` keys, and
    the interfaces parsed out of the vendored WIT by ``wasmdown.wit``.

Nothing here scrapes prose documentation, and vendored-file presence is
never treated as implementation.

Opcode coverage is NOT scraped from source: it is read from
``tests/backend-parity-results.jsonl``, produced by the backend-parity
lane (``make report-backend-parity``). When that file is absent the
report says "lane not run" instead of inventing rows.

Usage:
    python3 tools/backend-inventory.py             # write artifacts
    python3 tools/backend-inventory.py --check     # verify artifacts
"""

import json
import os
import re
import subprocess
import sys
import tomllib
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SPECS = os.path.join(ROOT, "specs")
HELPER = os.path.join(ROOT, "tools", "backend-inventory.lisp")
CENSUS = os.path.join(SPECS, "wasi", "preview1", "INVENTORY.md")
PARITY_RESULTS = os.path.join(ROOT, "tests", "backend-parity-results.jsonl")
JSON_OUT = os.path.join(ROOT, "tests", "backend-inventory.json")
MD_OUT = os.path.join(ROOT, "docs", "conformance", "inventory.md")

SCHEMA = "wasmdown.inventory.v1"
P2_PACKAGES = ("io", "clocks", "random", "cli", "filesystem", "sockets", "http")
P3_PACKAGES = ("clocks", "random", "cli", "filesystem", "sockets", "http")

# wasi:http is removed, not pending (specs/lock.toml [status] wasi-http).
EXCLUDED_PACKAGES = {"http"}
# P3-only exclusions: the providers stay in the registry for internal
# composition (the pinned command world imports sockets itself), but the
# interfaces are never a guest-visible support surface.
P3_INTERNAL_PACKAGES = {"http", "sockets"}
EXCLUDED_REASON = {
    "http": "excluded by decision: wasi:http removed from the runtime "
            "(specs/lock.toml [status] wasi-http = REMOVED)",
}

VERDICTS = {
    "bound": "bound",
    "excluded": "excluded by decision",
    "unstable": "unstable (gated off)",
    "export_only": "export-only (world export, not an import)",
    "types_only": "types-only (no host binding needed)",
    "excluded_internal": "excluded (internal composition only)",
    "missing": "not implemented",
}


def run_helper():
    out = subprocess.run(
        ["sbcl", "--script", HELPER],
        cwd=ROOT, check=True, capture_output=True, text=True,
    )
    records = []
    for line in out.stdout.splitlines():
        if line.startswith("[") or not line.strip():
            continue
        fields = line.split("\t")
        records.append(fields)
    return records


def parse_records(records):
    data = {
        "sbcl": None, "commit": None, "tree": None, "clean": None,
        "p1_sig": [], "p1_impl": [],
        "p2_vend": {}, "p2_bound": {}, "p2_export": {},
        "p3_vend": [], "p3_bound": [], "p3_internal": [], "p3_export": [],
    }
    for row in records:
        kind = row[0]
        if kind == "SBCL":
            data["sbcl"] = row[1]
        elif kind == "COMMIT":
            data["commit"] = row[1]
        elif kind == "P1-SIG":
            data["p1_sig"].append(row[1])
        elif kind == "P1-IMPL":
            data["p1_impl"].append(row[1])
        elif kind == "P2-VEND":
            name, stability = row[2], row[3]
            data["p2_vend"].setdefault(row[1], {})[name] = stability
        elif kind == "P2-BOUND":
            data["p2_bound"].setdefault(row[1], []).append(row[2])
        elif kind == "P2-EXPORT":
            data["p2_export"].setdefault(row[1], []).append(row[2])
        elif kind == "P3-VEND":
            data["p3_vend"].append(
                {"package": row[1], "interface": row[2],
                 "stability": row[3], "surface": row[4]})
        elif kind == "P3-BOUND":
            data["p3_bound"].append(row[2])
        elif kind == "P3-INTERNAL":
            data["p3_internal"].append(row[2])
        elif kind == "P3-EXPORT":
            data["p3_export"].append(row[2])
    _commit, tree, clean = read_source()
    data["tree"] = tree
    data["clean"] = clean
    return data


def read_pins():
    with open(os.path.join(SPECS, "lock.toml"), "rb") as fh:
        lock = tomllib.load(fh)
    core = lock.get("spec", {}).get("core", {}).get("commit")
    wasi = lock.get("spec", {}).get("wasi", {})
    p1 = wasi.get("preview1", {})
    p2 = wasi.get("preview2", {})
    return {
        "core_spec_commit": core,
        "wasi_release": wasi.get("release"),
        "wasi_release_date": wasi.get("release_date"),
        "wasi_release_sha256": wasi.get("sha256"),
        "preview1_commit": p1.get("commit"),
        "preview1_sha256_wasi_snapshot_preview1_witx":
            p1.get("sha256_wasi_snapshot_preview1_witx"),
        "preview2_default": p2.get("default"),
        "preview2_release_range": p2.get("release_range"),
        "lock_file": "specs/lock.toml",
    }


def census_names():
    """The 46 pinned Preview1 exports from the generated census."""
    names = []
    with open(CENSUS) as fh:
        for line in fh:
            m = re.match(r"\| `([a-z0-9_]+)` \|", line)
            if m:
                names.append(m.group(1))
    return names




def package_of(name):
    """wasi:<pkg>/<iface>@<ver> -> <pkg>."""
    return name.split(":", 1)[1].split("/", 1)[0]


def p1_section(data):
    pinned = sorted(set(census_names()))
    provided = sorted(set(data["p1_impl"]))
    sigs = set(data["p1_sig"])
    impls = set(provided)
    return {
        "pinned": pinned,
        "provided": provided,
        "missing_in_runtime": sorted(set(pinned) - impls),
        "extra_in_runtime": sorted(impls - set(pinned)),
        # the binding table and the implementation table disagreeing would
        # be a defect; recorded so a diff can never hide in prose
        "signature_only": sorted(sigs - impls),
        "implementation_only": sorted(impls - sigs),
    }


def p2_status(name, record, bound, exported):
    if name in bound:
        return "bound"
    if package_of(name) in EXCLUDED_PACKAGES:
        return "excluded"
    if record[name] == "unstable":
        return "unstable"
    if name in exported:
        return "export_only"
    return "missing"


def p2_section(data):
    releases = []
    for version in sorted(data["p2_vend"],
                           key=lambda v: [int(x) for x in v.split(".")]):
        record = data["p2_vend"][version]
        bound = set(data["p2_bound"].get(version, []))
        exported = set(data["p2_export"].get(version, []))
        rows = []
        for name in sorted(record):
            status = p2_status(name, record, bound, exported)
            rows.append({"interface": name, "status": status,
                         "stability": record[name]})
        pinned_stable = [r["interface"] for r in rows
                         if r["stability"] == "stable"]
        unbound_stable = [r["interface"] for r in rows
                          if r["stability"] == "stable"
                          and r["status"] != "bound"]
        releases.append({
            "version": version,
            "pinned_interfaces": sorted(record),
            "pinned_stable": sorted(pinned_stable),
            "bound": sorted(n for n in record if n in bound),
            "unbound_stable": sorted(unbound_stable),
            "missing": sorted(r["interface"] for r in rows
                              if r["status"] == "missing"),
            "excluded": sorted(r["interface"] for r in rows
                               if r["status"] == "excluded"),
            "unstable": sorted(r["interface"] for r in rows
                               if r["status"] == "unstable"),
            "export_only": sorted(r["interface"] for r in rows
                                  if r["status"] == "export_only"),
            "interfaces": rows,
        })
    default = next((r for r in releases if r["version"] == "0.2.12"),
                   releases[-1])
    return {
        "version": default["version"],
        "pinned_interfaces": default["pinned_interfaces"],
        "pinned_stable": default["pinned_stable"],
        "bound": default["bound"],
        "missing": default["missing"],
        "excluded": default["excluded"],
        "unstable": default["unstable"],
        "export_only": default["export_only"],
        "unbound_stable": default["unbound_stable"],
        "releases": releases,
    }


def p3_section(data):
    bound = set(data["p3_bound"])
    internal = set(data["p3_internal"])
    exported = set(data["p3_export"])
    packages = []
    for package in P3_PACKAGES:
        rows = []
        for rec in data["p3_vend"]:
            if rec["package"] != package:
                continue
            name = rec["interface"]
            if name in internal:
                status = "excluded_internal"
            elif name in bound:
                status = "bound"
            elif package in EXCLUDED_PACKAGES:
                status = "excluded"
            elif rec["stability"] == "unstable":
                status = "unstable"
            elif name in exported:
                status = "export_only"
            elif rec["surface"] == "types-only":
                status = "types_only"
            else:
                status = "missing"
            rows.append({"interface": name, "status": status,
                         "stability": rec["stability"],
                         "surface": rec["surface"]})
        rows.sort(key=lambda r: r["interface"])
        packages.append({
            "package": "wasi:" + package,
            "version": "0.3.2",
            "interfaces": [r["interface"] for r in rows],
            "provided": [r["interface"] for r in rows if r["status"] == "bound"],
            "missing": [r["interface"] for r in rows if r["status"] == "missing"],
            "excluded": [r["interface"] for r in rows
                         if r["status"] in ("excluded", "unstable",
                                             "excluded_internal")],
            "internal_only": [r["interface"] for r in rows
                              if r["status"] == "excluded_internal"],
            "export_only": [r["interface"] for r in rows
                            if r["status"] == "export_only"],
            "types_only": [r["interface"] for r in rows
                           if r["status"] == "types_only"],
            "rows": rows,
        })
    return packages


OPCODE_RE = re.compile(r"no lowering for opcode ([\w.:\-\[\]]+)")


def opcode_section():
    """Refused-opcode evidence from the backend-parity lane, or 'not run'."""
    if not os.path.exists(PARITY_RESULTS):
        return {
            "source": "tests/backend-parity-results.jsonl",
            "status": "lane not run",
            "pairs": 0,
            "refusals": 0,
            "refused_opcodes": [],
        }
    pairs = refusals = 0
    counts = {}
    with open(PARITY_RESULTS) as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            rec = json.loads(line)
            pairs += 1
            compiled = rec.get("compiled") or {}
            if (compiled.get("outcome") == "fail"
                    and compiled.get("phase") == "compile"):
                refusals += 1
                m = OPCODE_RE.search(compiled.get("detail") or "")
                if m:
                    counts[m.group(1)] = counts.get(m.group(1), 0) + 1
    return {
        "source": "tests/backend-parity-results.jsonl",
        "status": "present",
        "pairs": pairs,
        "refusals": refusals,
        "refused_opcodes": [{"opcode": k, "count": counts[k]}
                            for k in sorted(counts)],
    }


def git_field(*args):
    try:
        out = subprocess.run(("git",) + args, capture_output=True, text=True,
                             cwd=ROOT, check=False)
        return out.stdout.strip() or "unknown"
    except OSError:
        return "unknown"


def read_source():
    """Commit, tree hash and whether the tested tree was clean when the
    measurement started (this script writes artifacts afterwards)."""
    commit = git_field("rev-parse", "HEAD")
    tree = git_field("rev-parse", "HEAD^{tree}")
    clean = git_field("status", "--porcelain") in ("", "unknown")
    return commit, tree, clean


def build_document(data):
    p1 = p1_section(data)
    p2 = p2_section(data)
    p3 = p3_section(data)
    return {
        "schema": SCHEMA,
        "generated_at": datetime.now(timezone.utc)
                        .strftime("%Y-%m-%dT%H:%M:%SZ"),
        "sbcl_version": data["sbcl"],
        "source_commit": data["commit"],
        "source_tree": data["tree"],
        "clean_tree": data["clean"],
        "pins": read_pins(),
        "definitions": {
            "pinned_interfaces": "every interface parsed out of the vendored "
                                 "release tree by wasmdown.wit",
            "pinned_stable": "pinned_interfaces without an "
                             "@unstable(feature = ...) annotation",
            "bound": "present in the runtime registry for that exact release "
                     "(p2-builder-table / %wasi-provider-builders)",
            "excluded": "pinned but withheld by an explicit decision "
                        "(wasi:http removed; wasi:sockets network scope; "
                        "@unstable feature-gated off)",
            "internal_only": "a provider exists ONLY to compose the pinned "
                             "command world; guests may not import it "
                             "(wasi:sockets, wasi:http)",
            "export_only": "the world EXPORTS the interface, so it is not an "
                           "import and never gets a provider entry",
            "missing": "pinned, stable, and neither bound nor excluded nor an "
                       "export",
            "types_only": "interface declares no function and no resource; a "
                          "guest resolves its types structurally",
        },
        "P1": p1,
        "P2": p2,
        "P3": p3,
        "opcodes": opcode_section(),
    }


def md_table(header, rows):
    out = ["| " + " | ".join(header) + " |",
           "|" + "|".join("---" for _ in header) + "|"]
    out.extend("| " + " | ".join(r) + " |" for r in rows)
    return out


def render_markdown(doc):
    p1, p2, p3, ops = doc["P1"], doc["P2"], doc["P3"], doc["opcodes"]
    pins = doc["pins"]
    out = []
    add = out.append
    add_all = out.extend
    add("# Backend / ABI inventory")
    add("")
    add("Generated by `python3 tools/backend-inventory.py`; do not edit by hand. "
        "Machine-readable twin: [`tests/backend-inventory.json`]"
        "(../../tests/backend-inventory.json) (schema `%s`)." % doc["schema"])
    add("")
    add("Pinned: WASI Preview1 at `%s`; WASI 0.2 releases `%s` (default `%s`); "
        "WASI 0.3.2 release `%s`; core spec `%s`."
        % (pins["preview1_commit"], pins["preview2_release_range"],
           pins["preview2_default"], pins["wasi_release"],
           pins["core_spec_commit"]))
    add("")
    add("## Method")
    add("")
    add("Two sources only, and neither is prose. **Pinned:** the generated "
        "Preview1 census (`specs/wasi/preview1/INVENTORY.md`, from the pinned "
        "WITX) and the vendored WIT trees (`specs/wasi/wasi-wit-<release>/`, "
        "parsed by `wasmdown.wit`). **Runtime:** `tools/backend-inventory.lisp` "
        "loads the system in SBCL and prints the registration tables that "
        "actually bind — `*p1-signatures*` / `*p1-implementations*`, "
        "`p2-builder-table` per release, `%%wasi-provider-builders` — plus the "
        "interfaces the parser reads and the worlds' exports. A vendored file "
        "is never evidence of implementation, and a name is `bound` only when "
        "it appears in the runtime registry for that exact release. Opcode "
        "coverage is not scraped from source: it is read from the backend-"
        "parity lane's result records. Generated %s from commit `%s`, SBCL %s."
        % (doc["generated_at"], doc["source_commit"], doc["sbcl_version"]))
    add("")

    add("## P1 — WASI Preview1 (46 pinned exports)")
    add("")
    add("Pinned %d, provided %d, missing in runtime %d, extra in runtime %d."
        % (len(p1["pinned"]), len(p1["provided"]),
           len(p1["missing_in_runtime"]), len(p1["extra_in_runtime"])))
    add("")
    add("`provided` is `*p1-implementations*`; the signature table and the "
        "implementation table differ by %d / %d names. `bound` means the "
        "runtime has an entry for the import and checks the pinned WITX "
        "signature at link time, not that every path is semantically "
        "complete (named limitations live in `specs/lock.toml`)."
        % (len(p1["signature_only"]), len(p1["implementation_only"])))
    add("")
    provided = set(p1["provided"])
    add_all(md_table(["export", "status"],
                  [[f"`{n}`", "bound" if n in provided else "not implemented"]
                   for n in p1["pinned"]]))
    add("")
    if p1["missing_in_runtime"] or p1["extra_in_runtime"]:
        add("Diff: missing `%s`; extra `%s`."
            % (", ".join(p1["missing_in_runtime"]) or "-",
               ", ".join(p1["extra_in_runtime"]) or "-"))
        add("")

    add("## P2 — WASI 0.2 stable interfaces")
    add("")
    add("Per release: interfaces parsed from the vendored tree, and the "
        "`p2-builder-table` registry for that exact version. `stable` drops "
        "`@unstable` annotations. The documentation claim is 27 of 31 stable "
        "interfaces bound at exact release names; the table below is the "
        "measurement, release by release.")
    add("")
    add_all(md_table(
        ["release", "pinned", "stable", "bound", "unbound stable", "missing"],
        [[r["version"], str(len(r["pinned_interfaces"])),
          str(len(r["pinned_stable"])), str(len(r["bound"])),
          str(len(r["unbound_stable"])), str(len(r["missing"]))]
         for r in p2["releases"]]))
    add("")
    drift = [(release["version"], name)
             for release in p2["releases"]
             for name in release["pinned_interfaces"]
             if not name.endswith("@" + release["version"])]
    if drift:
        add("Release-dir drift (a vendored package declares a different patch "
            "version): " + "; ".join("`%s` ships `%s`" % (rel, name)
                                     for rel, name in drift)
            + ". The pinned set is the PARSED name, so counts are unaffected.")
        add("")
    default = next(r for r in p2["releases"] if r["version"] == p2["version"])
    add("The %d unbound STABLE interfaces at `%s` are listed here by name; "
        "every one is accounted for (no silent gap), and the per-release "
        "tables below repeat the status for each release."
        % (len(default["unbound_stable"]), p2["version"]))
    add("")
    by_name = {r["interface"]: r["status"] for r in default["interfaces"]}
    add_all(md_table(["interface", "status"],
                     [[f"`{n}`", VERDICTS[by_name[n]]]
                      for n in default["unbound_stable"]]))
    add("")
    for release in p2["releases"]:
        add("### wasi:*@%s" % release["version"])
        add("")
        add_all(md_table(["interface", "status", "stability"],
                      [[f"`{r['interface']}`", VERDICTS[r["status"]],
                        r["stability"]] for r in release["interfaces"]]))
        add("")

    add("## P3 — WASI 0.3.2 packages")
    add("")
    add("Interfaces parsed from `specs/wasi/wasi-wit-0.3.2/`; `provided` is the "
        "`%wasi-provider-builders` registry. `wasi:http` and `wasi:sockets` "
        "(TCP/UDP/DNS) are excluded by decision; their WIT is reference data "
        "and any registry entry is internal composition only, never support.")
    add("")
    add_all(md_table(
        ["package", "interfaces", "provided", "excluded", "export-only",
         "types-only", "missing"],
        [[f"`{p['package']}@{p['version']}`", str(len(p["interfaces"])),
          str(len(p["provided"])), str(len(p["excluded"])),
          str(len(p["export_only"])), str(len(p["types_only"])),
          str(len(p["missing"]))] for p in p3]))
    add("")
    for package in p3:
        add("### `%s@%s`" % (package["package"], package["version"]))
        add("")
        add_all(md_table(["interface", "status", "surface"],
                      [[f"`{r['interface']}`", VERDICTS[r["status"]],
                        r["surface"]] for r in package["rows"]]))
        add("")

    add("## Exclusions")
    add("")
    add("`excluded by decision` is not the same thing as `not implemented`: "
        "the first is a choice with a reason, the second is a gap.")
    add("")
    # Collapsed by interface BASE and reason; exact version suffixes are in
    # the per-release tables above. A release whose vendored http package
    # lags (0.2.4 ships http@0.2.3) is still attributed correctly because
    # the grouping is over the release DIRS that contain the interface.
    def base(name):
        return name.split("@", 1)[0]

    def spans(releases):
        """0.2.1..0.2.12 + 0.3.2 -> "0.2.1-0.2.12, 0.3.2" (patch runs only)."""
        nums = sorted(releases, key=lambda v: [int(x) for x in v.split(".")])
        runs = []
        for ver in nums:
            major, minor, patch = (int(x) for x in ver.split("."))
            if runs:
                pmaj, pmin, ppat = runs[-1][-1]
                if (major, minor, patch) == (pmaj, pmin, ppat + 1):
                    runs[-1].append((major, minor, patch))
                    continue
            runs.append([(major, minor, patch)])
        out = []
        for run in runs:
            first = ".".join(map(str, run[0]))
            last = ".".join(map(str, run[-1]))
            out.append(first if len(run) == 1 else "%s-%s" % (first, last))
        return ", ".join(out)

    grouped = {}
    for release, rows in (
            [(r["version"], r["interfaces"]) for r in p2["releases"]]
            + [("0.3.2", package["rows"]) for package in p3]):
        for r in rows:
            if r["status"] not in ("excluded", "unstable"):
                continue
            reason = ("`wasi:http` removed (decision; `specs/lock.toml` "
                      "[status] wasi-http)" if r["status"] == "excluded"
                      else "`@unstable`, feature-gated off (`specs/lock.toml`)")
            grouped.setdefault((base(r["interface"]), reason), set()).add(release)
    excluded_rows = [[f"`{name}`", spans(releases), reason]
                     for (name, reason), releases in sorted(grouped.items())]
    not_impl_rows = [
        [f"`{n}`", "Preview1 pinned export absent from the runtime table"]
        for n in p1["missing_in_runtime"]]
    not_impl_rows += [
        [f"`{r['interface']}@{release['version']}`",
         "pinned and stable, no provider, not an export"]
        for release in p2["releases"] for r in release["interfaces"]
        if r["status"] == "missing"]
    not_impl_rows += [
        [f"`{r['interface']}`", "pinned and stable, no provider, not an export"]
        for package in p3 for r in package["rows"] if r["status"] == "missing"]
    add("### Excluded by decision")
    add("")
    add_all(md_table(["interface", "release dirs", "reason"],
                             excluded_rows)
              if excluded_rows else ["None."])
    add("")
    add("### Not implemented")
    add("")
    add_all(md_table(["interface", "why it is a gap"], not_impl_rows)
              if not_impl_rows else ["None."])
    add("")
    add("### Not an import")
    add("")
    not_import = {}
    for release, rows in (
            [(r["version"], r["interfaces"]) for r in p2["releases"]]
            + [("0.3.2", package["rows"]) for package in p3]):
        for r in rows:
            if r["status"] == "export_only":
                key = (base(r["interface"]),
                       "the command world EXPORTS it; the runtime serves the "
                       "export, so no import provider exists")
            elif r["status"] == "types_only":
                key = (base(r["interface"]),
                       "declares no function and no resource; a guest resolves "
                       "its types structurally")
            else:
                continue
            not_import.setdefault(key, set()).add(release)
    export_rows = [[f"`{name}`", spans(releases), why]
                   for (name, why), releases in sorted(not_import.items())]
    add_all(md_table(["interface", "release dirs", "why no provider entry"],
                     export_rows) if export_rows else ["None."])
    add("")

    add("## Baseline opcode coverage")
    add("")
    add("Evidence comes from the backend-parity lane "
        "(`tests/backend-parity-results.jsonl`, `wasmdown.backend-parity.v1`, "
        "written by `make report-backend-parity`), not from source text. A "
        "compiled `fail` at phase `compile` is a refusal to lower an "
        "instruction; the counts below are whatever that file's last run "
        "recorded.")
    add("")
    if ops["status"] == "lane not run":
        add("**Lane not run** — `tests/backend-parity-results.jsonl` is "
            "absent, so no opcode refusal counts are claimed here.")
    else:
        add("Pairs recorded: %d; compiled compile refusals: %d; distinct "
            "refused opcodes: %d."
            % (ops["pairs"], ops["refusals"], len(ops["refused_opcodes"])))
        add("")
        add_all(md_table(["opcode", "refusals"],
                       [[f"`{o['opcode']}`", str(o["count"])]
                        for o in ops["refused_opcodes"]])
              if ops["refused_opcodes"] else ["No refused opcodes recorded."])
    add("")

    add("## Regenerate")
    add("")
    add("```sh")
    add("python3 tools/backend-inventory.py            # writes both artifacts")
    add("python3 tools/backend-inventory.py --check    # fails if stale")
    add("```")
    add("")
    add("The script runs `sbcl --script tools/backend-inventory.lisp`; it needs "
        "no network access and reads only the pinned trees named above.")
    add("")
    return "\n".join(out)


def main():
    check = "--check" in sys.argv[1:]
    doc = build_document(parse_records(run_helper()))
    if check and os.path.exists(JSON_OUT):
        # the volatile fields (wall clock, and the source identity of the
        # tree doing the check) are reused from the committed document so
        # --check compares CONTENT: running it from a dirty or different
        # tree must not report the inventory stale.
        with open(JSON_OUT) as fh:
            old_doc = json.load(fh)
        for key in ("generated_at", "source_commit", "source_tree",
                    "clean_tree"):
            if key in old_doc:
                doc[key] = old_doc[key]
    js = json.dumps(doc, indent=1, sort_keys=False) + "\n"
    md = render_markdown(doc)
    stale = []
    for path, text in ((JSON_OUT, js), (MD_OUT, md)):
        old = open(path).read() if os.path.exists(path) else None
        if check:
            if old != text:
                stale.append(os.path.relpath(path, ROOT))
        else:
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as fh:
                fh.write(text)
            print("wrote %s" % os.path.relpath(path, ROOT))
    if stale:
        raise SystemExit("stale inventory: " + ", ".join(stale))
    if not check:
        p1, p2, p3 = doc["P1"], doc["P2"], doc["P3"]
        print("P1 pinned=%d provided=%d missing=%d extra=%d"
              % (len(p1["pinned"]), len(p1["provided"]),
                 len(p1["missing_in_runtime"]), len(p1["extra_in_runtime"])))
        d = next(r for r in p2["releases"] if r["version"] == p2["version"])
        print("P2 %s pinned=%d stable=%d bound=%d unbound_stable=%d"
              % (d["version"], len(d["pinned_interfaces"]),
                 len(d["pinned_stable"]), len(d["bound"]),
                 len(d["unbound_stable"])))
        print("P3 packages=%d interfaces=%d provided=%d excluded=%d"
              % (len(p3), sum(len(p["interfaces"]) for p in p3),
                 sum(len(p["provided"]) for p in p3),
                 sum(len(p["excluded"]) for p in p3)))


if __name__ == "__main__":
    main()
