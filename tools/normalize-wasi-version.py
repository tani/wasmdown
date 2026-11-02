#!/usr/bin/env python3
"""Rewrite WASI interface-version labels in a generated component.

The pinned Rust target emits the WASI 0.3 line as @0.3.0 (its std snapshot)
while this repository vendors @0.3.2. The component model encodes the version
only in import/export NAME strings; the interface shapes are validated to be
identical, so the build normalizes the labels to the pinned release and then
runs wasm-tools validate. Same-length replacement only: no offsets change.
"""
import sys


def main(argv):
    if len(argv) != 5:
        print("usage: normalize-wasi-version.py IN OUT OLD NEW", file=sys.stderr)
        return 2
    src, dst, old, new = argv[1:]
    if len(old) != len(new):
        print("version labels must have equal length", file=sys.stderr)
        return 2
    data = open(src, "rb").read()
    count = data.count(old.encode())
    if count == 0:
        print(f"no {old} labels found", file=sys.stderr)
        return 1
    open(dst, "wb").write(data.replace(old.encode(), new.encode()))
    print(f"normalized {count} labels: {old} -> {new}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
