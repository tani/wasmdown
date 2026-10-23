#!/usr/bin/env python3
"""One-off: bucket official-suite failure details (read-only analysis).

Groups failing commands from tests/spec-core-results.jsonl by their
detail-message prefix to expose systematic clusters.
"""
import json
import collections
import sys

FILES = sys.argv[1:] or [
    "f32", "f64", "f32_cmp", "f64_cmp", "conversions",
    "float_exprs", "float_misc", "names", "f32_bitwise", "f64_bitwise",
]

buckets = collections.Counter()
samples = {}
with open("tests/spec-core-results.jsonl") as fh:
    for line in fh:
        d = json.loads(line)
        if d["outcome"] != "fail":
            continue
        fname = d["case_id"].split("::")[0]
        if fname not in FILES:
            continue
        det = " ".join((d["detail"] or "").split())
        key = det[:60]
        buckets[key] += 1
        samples.setdefault(key, d["case_id"])

for key, count in buckets.most_common(15):
    print(f"{count:6d}  [{samples[key]}]  {key!r}")
