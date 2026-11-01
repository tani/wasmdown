#!/usr/bin/env bash
# Set up the host-side environment for WASI network tests
# (docs/development/network-testing.md).
#
# Installs into the git-ignored .cache/ directory:
#   .cache/wasi-testsuite        pinned WebAssembly/wasi-testsuite tree
#   .cache/tools/wasmtime-*      reference Wasmtime (harness smoke only)
# The wasmdown runtime itself is exercised through its normal CLI; this
# script never installs or patches the runtime.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PIN=9d84631e427729802085c3d172d0a63cd360a1fa
BRANCH=prod/testsuite-base
REPO=https://github.com/WebAssembly/wasi-testsuite
WASMTIME_VERSION=50.0.1
SUITE_DIR="${WASI_TESTSUITE_DIR:-$ROOT/.cache/wasi-testsuite}"
TOOLS_DIR="$ROOT/.cache/tools"

say() { printf '%s\n' "$*"; }
die() { printf 'error: %s\n' "$*" >&2; exit 1; }

# ---- pinned upstream suite ------------------------------------------------
if [ ! -d "$SUITE_DIR/.git" ]; then
  say "cloning wasi-testsuite ($BRANCH) into $SUITE_DIR"
  mkdir -p "$(dirname "$SUITE_DIR")"
  git clone --branch "$BRANCH" --single-branch "$REPO" "$SUITE_DIR"
fi
git -C "$SUITE_DIR" fetch --depth 1 origin "$PIN" >/dev/null 2>&1 || true
git -C "$SUITE_DIR" checkout --detach "$PIN" >/dev/null 2>&1 \
  || die "cannot check out pinned commit $PIN in $SUITE_DIR"
HEAD="$(git -C "$SUITE_DIR" rev-parse HEAD)"
[ "$HEAD" = "$PIN" ] || die "suite HEAD $HEAD != pinned $PIN"

# ---- Python runner dependencies ------------------------------------------
python3 - <<'PY' || die "install the upstream runner dependencies: python3 -m pip install colorama requests"
import colorama, requests  # noqa: F401
PY

# ---- loopback + HTTP framing smoke ---------------------------------------
python3 - <<'PY' || die "loopback HTTP smoke failed"
import http.server, socket, threading, urllib.request
s = socket.socket(); s.bind(("127.0.0.1", 0)); port = s.getsockname()[1]; s.close()
class H(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        body = b"wmd-network-ready"
        self.send_response(200); self.send_header("Content-Length", str(len(body)))
        self.end_headers(); self.wfile.write(body)
    def log_message(self, *a): pass
srv = http.server.ThreadingHTTPServer(("127.0.0.1", 0), H)
threading.Thread(target=srv.serve_forever, daemon=True).start()
host, port = srv.server_address
with urllib.request.urlopen(f"http://{host}:{port}/", timeout=5) as r:
    assert r.status == 200 and r.read() == b"wmd-network-ready"
srv.shutdown(); srv.server_close()
PY

# ---- reference Wasmtime (optional but installed by default) ---------------
ARCH="$(uname -m)"
case "$ARCH" in
  aarch64|arm64) WT_ARCH=aarch64 ;;
  x86_64|amd64)  WT_ARCH=x86_64 ;;
  *) die "unsupported architecture for the reference runtime: $ARCH" ;;
esac
WT_DIR="$TOOLS_DIR/wasmtime-v${WASMTIME_VERSION}-${WT_ARCH}-linux"
if [ ! -x "$WT_DIR/wasmtime" ]; then
  mkdir -p "$TOOLS_DIR"
  TAR="$TOOLS_DIR/wasmtime-v${WASMTIME_VERSION}-${WT_ARCH}-linux.tar.xz"
  if [ ! -f "$TAR" ]; then
    say "downloading Wasmtime $WASMTIME_VERSION ($WT_ARCH)"
    curl -fsSL -o "$TAR" \
      "https://github.com/bytecodealliance/wasmtime/releases/download/v${WASMTIME_VERSION}/wasmtime-v${WASMTIME_VERSION}-${WT_ARCH}-linux.tar.xz"
  fi
  tar -xf "$TAR" -C "$TOOLS_DIR"
fi
[ -x "$WT_DIR/wasmtime" ] || die "reference Wasmtime missing at $WT_DIR/wasmtime"
"$WT_DIR/wasmtime" --version >/dev/null || die "reference Wasmtime does not run"

cat <<MSG

Network test environment ready.

  export WASI_TESTSuite_DIR="$SUITE_DIR"
  export WASMTIME="$WT_DIR/wasmtime"

  make test-wasi-network             # wasmdown adapter (expected red until support lands)
  make test-wasi-network-reference   # reference Wasmtime harness smoke
MSG
