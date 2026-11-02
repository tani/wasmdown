#!/usr/bin/env bash
# Install the pinned WASI 0.3 guest toolchain under the git-ignored .cache/.
# The generated artifact and its hash are produced by
# tools/build-toolchain-guest.sh; nothing here changes the runtime.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUSTUP_HOME="$ROOT/.cache/rustup"
CARGO_HOME="$ROOT/.cache/cargo"
export RUSTUP_HOME CARGO_HOME
export PATH="$CARGO_HOME/bin:$PATH"
CHANNEL=nightly-2026-11-01
TARGET=wasm32-wasip3
TOOLS="$ROOT/.cache/tools"
WASM_TOOLS_VERSION=1.263.0
ARCH="$(uname -m)"
case "$ARCH" in
  aarch64|arm64) ARCH_TAG=aarch64 ;;
  x86_64|amd64) ARCH_TAG=x86_64 ;;
  *) echo "unsupported host architecture: $ARCH" >&2; exit 2 ;;
esac

mkdir -p "$RUSTUP_HOME" "$CARGO_HOME" "$TOOLS"

if [ ! -x "$CARGO_HOME/bin/rustup" ]; then
  echo "installing rustup into $CARGO_HOME"
  curl -fsSL -o /tmp/rustup-init https://sh.rustup.rs
  chmod +x /tmp/rustup-init
  /tmp/rustup-init -y --profile minimal --default-toolchain "$CHANNEL" \
    --target "$TARGET" >/dev/null
else
  rustup toolchain install "$CHANNEL" --profile minimal --target "$TARGET" >/dev/null
  rustup target add --toolchain "$CHANNEL" "$TARGET" >/dev/null
fi
rustc "+$CHANNEL" --version
rustup target list --toolchain "$CHANNEL" --installed | grep -qx "$TARGET"

WT_DIR="$TOOLS/wasm-tools-$WASM_TOOLS_VERSION-$ARCH_TAG-linux"
if [ ! -x "$WT_DIR/wasm-tools" ]; then
  TAR="$TOOLS/wasm-tools-$WASM_TOOLS_VERSION-$ARCH_TAG-linux.tar.gz"
  if [ ! -f "$TAR" ]; then
    curl -fsSL -o "$TAR" \
      "https://github.com/bytecodealliance/wasm-tools/releases/download/v$WASM_TOOLS_VERSION/wasm-tools-$WASM_TOOLS_VERSION-$ARCH_TAG-linux.tar.gz"
  fi
  tar -xf "$TAR" -C "$TOOLS"
fi
"$WT_DIR/wasm-tools" --version

cat <<MSG

WASI guest toolchain ready:
  export RUSTUP_HOME="$RUSTUP_HOME"
  export CARGO_HOME="$CARGO_HOME"
  export PATH="$CARGO_HOME/bin:\$PATH"
  export WASM_TOOLS="$WT_DIR/wasm-tools"
  make build-toolchain-guest
  make test-toolchain-guest
MSG
