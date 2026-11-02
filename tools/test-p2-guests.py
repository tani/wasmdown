#!/usr/bin/env python3
"""Run real WASI 0.2 socket and HTTP proxy guests through the CLI.

Fixtures are built by tools/setup-p2-guests.sh and pinned by
tests/fixtures/wasi/P2-TOOLCHAIN-CHECKSUMS.
"""
import http.client
import pathlib
import socket
import socketserver
import subprocess
import sys
import threading
import time

ROOT = pathlib.Path(__file__).resolve().parent.parent
FIX = ROOT / "tests" / "fixtures" / "wasi"
SOCK = FIX / "p2-sock-guest.wasm"
HTTP = FIX / "p2-http-guest.wasm"


def run(cmd, **kw):
    return subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, **kw)


def socket_case():
    received = []
    server = socket.socket()
    server.bind(("127.0.0.1", 0))
    server.listen(1)
    port = server.getsockname()[1]

    def serve():
        c, _ = server.accept()
        data = c.recv(16)
        received.append(data)
        c.sendall(data)
        c.close()

    t = threading.Thread(target=serve, daemon=True)
    t.start()
    proc = run(["sbcl", "--script", "tools/wasmdown.lisp", "run",
                str(SOCK), "--wasi", "--net", f"127.0.0.1:{port}",
                "--", str(port)])
    server.close()
    t.join(2)
    return proc, received


def http_case():
    proc = subprocess.Popen(
        ["sbcl", "--script", "tools/wasmdown.lisp", "serve", str(HTTP),
         "--wasi", "--net", "*:*", "--listen", "*:*", "--dns", "*",
         "--addr", "127.0.0.1:0"],
        cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    try:
        line = proc.stderr.readline().strip()
        if not line.startswith("http://"):
            return proc, None, f"no URL line: {line!r}"
        rest = line[len("http://"):]
        host, port = rest.rsplit(":", 1)
        conn = http.client.HTTPConnection(host, int(port), timeout=10)
        conn.request("GET", "/hello")
        resp = conn.getresponse()
        body = resp.read()
        code = resp.status
        conn.close()
        return proc, (code, body), None
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


def main():
    for path in (SOCK, HTTP):
        if not path.exists():
            print(f"missing {path}; run tools/setup-p2-guests.sh", file=sys.stderr)
            return 2
    proc, received = socket_case()
    if proc.returncode != 0 or proc.stdout.strip() != "p2-sock:ping":
        print(f"P2 socket FAIL exit={proc.returncode} out={proc.stdout!r} "
              f"err={proc.stderr[-800:]!r}", file=sys.stderr)
        return 1
    print("P2 socket: PASS (tcp echo)")

    proc, result, error = http_case()
    if error or result != (200, b"p2-proxy-ok"):
        print(f"P2 HTTP FAIL {error or result} err={proc.stderr.read()[-800:]!r}",
              file=sys.stderr)
        return 1
    print("P2 HTTP proxy guest: PASS (GET /hello -> 200 p2-proxy-ok)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
