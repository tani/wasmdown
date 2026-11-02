#!/usr/bin/env python3
"""Run the generated WASI 0.3 command guest in both backends.

The binary is produced by tools/build-toolchain-guest.sh and pinned by
tests/fixtures/wasi/TOOLCHAIN-CHECKSUMS. Exit 0 only when the hash, the
component preamble and the exact stdout match.
"""
import hashlib
import os
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
WASM = ROOT / "tests" / "fixtures" / "wasi" / "toolchain-guest.wasm"
SUMS = ROOT / "tests" / "fixtures" / "wasi" / "TOOLCHAIN-CHECKSUMS"
EXPECTED = "toolchain-guest:2:env-marker\n"


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    if not WASM.exists():
        print(f"missing {WASM}; run tools/setup-wasi-toolchain.sh and "
              f"make build-toolchain-guest", file=sys.stderr)
        return 2
    if not SUMS.exists():
        print(f"missing {SUMS}", file=sys.stderr)
        return 2
    pinned = SUMS.read_text().split()[0]
    actual = digest(WASM)
    if pinned != actual:
        print(f"toolchain guest hash drift: {actual} != pinned {pinned}",
              file=sys.stderr)
        return 1
    if WASM.read_bytes()[:4] != b"\0asm":
        print("not a wasm/component binary", file=sys.stderr)
        return 1
    for backend in ("interpret", "compiled"):
        env = dict(os.environ)
        env["WASMDOWN_API_MODE"] = backend
        cmd = ["sbcl", "--script", "tools/wasmdown.lisp", "run",
               str(WASM), "--wasi", "--env", "WMD_TOOLCHAIN=env-marker",
               "--", "x"]
        proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True,
                              env=env)
        if proc.returncode != 0 or proc.stdout != EXPECTED:
            print(f"{backend}: FAIL exit={proc.returncode} stdout={proc.stdout!r} "
                  f"stderr={proc.stderr[-800:]!r}", file=sys.stderr)
            return 1
        print(f"{backend}: PASS")
    print(f"toolchain guest: PASS sha256 {actual}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
