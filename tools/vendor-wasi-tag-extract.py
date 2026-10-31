#!/usr/bin/env python3
"""Extract the stable WASI 0.2 WIT source from a WebAssembly/WASI tag tarball.

The tag trees move over time (``preview2/`` in v0.2.0, ``wasip2/`` in
v0.2.1..v0.2.8, ``proposals/<pkg>/wit/`` in v0.2.9). This helper copies the
stable WIT files verbatim into a NORMALIZED ``<pkg>/<interface>.wit`` layout
so the pins for old releases are usable by the multi-file loader without
runtime layout guessing. The index file records the resulting paths.
"""
import os
import shutil
import sys
import tarfile

PKGS = {"cli", "clocks", "filesystem", "http", "io", "random", "sockets"}


def main(tarpath, dest, version):
    prefix = f"WASI-{version}/"
    with tarfile.open(tarpath) as tf:
        names = tf.getnames()
        bases = [prefix + "preview2/", prefix + "wasip2/"]
        base = next((b for b in bases if any(n.startswith(b) for n in names)), None)
        copied = 0
        for name in names:
            if not name.endswith(".wit"):
                continue
            rel = None
            if base and name.startswith(base):
                rel = name[len(base):]
            elif name.startswith(prefix + "proposals/") and "/wit/" in name:
                pkg, rest = name[len(prefix + "proposals/"):].split("/wit/", 1)
                rel = pkg + "/" + rest
            if not rel:
                continue
            parts = rel.split("/")
            pkg = next((p for p in parts[:-1] if p in PKGS), None)
            if pkg is None:
                raise SystemExit(f"cannot derive package for {name}")
            out = os.path.join(dest, rel)
            os.makedirs(os.path.dirname(out), exist_ok=True)
            with tf.extractfile(name) as src, open(out, "wb") as f:
                shutil.copyfileobj(src, f)
            copied += 1
    if not copied:
        raise SystemExit(f"no WIT files found in {tarpath}")
    print(f"{version}: {copied} WIT files")


if __name__ == "__main__":
    main(*sys.argv[1:4])
