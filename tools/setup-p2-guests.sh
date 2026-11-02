#!/usr/bin/env bash
# Build real WASI 0.2 guests for the sockets + HTTP network lanes.
# Development/test only: tools live under the git-ignored .cache/.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SDK="$ROOT/.cache/tools/wasi-sdk-34.0-arm64-linux"
CARGO_HOME="${CARGO_HOME:-$ROOT/.cache/cargo}"
RUSTUP_HOME="${RUSTUP_HOME:-$ROOT/.cache/rustup}"
export CARGO_HOME RUSTUP_HOME
export PATH="$CARGO_HOME/bin:$PATH"
OUT="$ROOT/tests/fixtures/wasi"
mkdir -p "$OUT"

if [ ! -x "$SDK/bin/clang" ]; then
  echo "missing wasi-sdk 34; run tools/setup-curl-wasm.sh first (it installs it)" >&2
  exit 2
fi
"$SDK/bin/clang" --target=wasm32-wasip2 -O2 \
  "$ROOT/tests/toolchain/p2-sock-guest.c" -o "$OUT/p2-sock-guest.wasm"

if ! command -v cargo-component >/dev/null 2>&1; then
  echo "missing cargo-component; install with: cargo install cargo-component --locked" >&2
  exit 2
fi
(cd "$ROOT/tests/toolchain/p2-http-guest" && cargo component build --release)
cp "$ROOT/tests/toolchain/p2-http-guest/target/wasm32-wasip1/release/p2proxy.wasm" \
   "$OUT/p2-http-guest.wasm"

(cd "$OUT" && sha256sum p2-sock-guest.wasm p2-http-guest.wasm > P2-TOOLCHAIN-CHECKSUMS)
cat "$OUT/P2-TOOLCHAIN-CHECKSUMS"
