#!/usr/bin/env python3
"""Run the wasi-sdk-built curl component against a loopback HTTP server.

The curl binary is built by tools/setup-curl-wasm.sh. Verbose mode is used
because the current async scheduler can leave quiet mode parked after the
last body byte; -v/trace runs complete reliably (quiet + --max-time is
timing-dependent). The residual is recorded in the history.
"""
import http.server
import pathlib
import socket
import socketserver
import subprocess
import sys
import threading
import tempfile

ROOT = pathlib.Path(__file__).resolve().parent.parent
CURL = ROOT / ".cache" / "tools" / "curl-8.22.0" / "build-wasi" / "src" / "curl"
BODY = b"hello from curl wasm\n"


def main():
    if not CURL.exists():
        print(f"missing {CURL}; run tools/setup-curl-wasm.sh", file=sys.stderr)
        return 2
    with tempfile.TemporaryDirectory() as tmp:
        path = pathlib.Path(tmp) / "payload.txt"
        path.write_bytes(BODY)
        handler = lambda *a, **k: http.server.SimpleHTTPRequestHandler(
            *a, directory=tmp, **k)
        with socketserver.TCPServer(("127.0.0.1", 0), handler) as server:
            port = server.server_address[1]
            threading.Thread(target=server.serve_forever, daemon=True).start()
            cmd = ["sbcl", "--script", "tools/wasmdown.lisp", "run",
                   str(CURL), "--wasi", "--net", f"127.0.0.1:{port}", "--",
                   "-sS", "-v", f"http://127.0.0.1:{port}/payload.txt"]
            proc = subprocess.run(cmd, cwd=ROOT, capture_output=True)
            server.shutdown()
        if proc.returncode != 0 or proc.stdout != BODY:
            print(f"curl wasm FAIL exit={proc.returncode} "
                  f"stdout={proc.stdout!r} stderr={proc.stderr[-1200:]!r}",
                  file=sys.stderr)
            return 1
        print(f"curl wasm: PASS ({len(proc.stdout)} bytes over loopback)")
        return 0


if __name__ == "__main__":
    sys.exit(main())
