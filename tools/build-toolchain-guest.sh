#!/usr/bin/env bash
# Build the generated WASI 0.3 command guest from tests/toolchain/wasi-guest
# and pin its hash; run with tools/setup-wasi-toolchain.sh first.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUSTUP_HOME="${RUSTUP_HOME:-$ROOT/.cache/rustup}"
CARGO_HOME="${CARGO_HOME:-$ROOT/.cache/cargo}"
export RUSTUP_HOME CARGO_HOME
export PATH="$CARGO_HOME/bin:$PATH"
CRATE="$ROOT/tests/toolchain/wasi-guest"
OUT="$ROOT/tests/fixtures/wasi/toolchain-guest.wasm"
SUMS="$ROOT/tests/fixtures/wasi/TOOLCHAIN-CHECKSUMS"
(cd "$CRATE" && cargo build --release --target wasm32-wasip3)
RAW="$CRATE/target/wasm32-wasip3/release/wmd-toolchain-guest.wasm"
# Rust's wasm32-wasip3 std currently emits @0.3.0 labels; the vendored
# release is @0.3.2 and the interface shapes are validated to be identical.
# Normalize the version labels (same-length) before pinning the artifact.
python3 "$ROOT/tools/normalize-wasi-version.py" "$RAW" "$OUT" 0.3.0 0.3.2
WASM_TOOLS="${WASM_TOOLS:-$ROOT/.cache/tools/wasm-tools-1.263.0-$(uname -m | sed 's/aarch64/aarch64/;s/x86_64/x86_64/')-linux/wasm-tools}"
if [ -x "$WASM_TOOLS" ]; then "$WASM_TOOLS" validate "$OUT"; fi
sha256sum "$OUT" | sed 's# .*/# #' > "$SUMS"
cat "$SUMS"
