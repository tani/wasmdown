#!/usr/bin/env bash
# Build the WASI fixture artifacts (development only).
#
# Every input is pinned in specs/lock.toml; this script verifies the input
# hashes, builds with the pinned toolchain (tools/wasi-toolchain.nix) and
# records the output hashes in tests/fixtures/wasi/CHECKSUMS.
#
# Reference tools are fixture BUILDERS. Nothing here is a runtime fallback:
# wasmdown executes the resulting modules itself.
#
# Usage:
#   nix shell --inputs-from . tools/wasi-toolchain.nix --command \
#     bash tools/build-wasi-fixtures.sh
#   tools/build-wasi-fixtures.sh --check    # verify recorded output hashes
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CACHE="$ROOT/.cache/wasi-fixtures"
OUT="$ROOT/tests/fixtures/wasi"
CHECK=0
[ "${1:-}" = "--check" ] && CHECK=1
mkdir -p "$CACHE" "$OUT"

SQLITE_URL=https://sqlite.org/2026/sqlite-amalgamation-3540100.zip
SQLITE_SHA=0a9c8e34352dad46006aa82836d5383bff9dc6bc9ce5a60553c05aa30e6ad1f9
ADAPTER_BASE=https://github.com/bytecodealliance/wasmtime/releases/download/v50.0.1
CMD_ADAPTER=wasi_snapshot_preview1.command.wasm
CMD_ADAPTER_SHA=0b4fe490390ecc9c2222f969f65b179e658a3327e005efd4ddc959766132a3c4
REACTOR_ADAPTER=wasi_snapshot_preview1.reactor.wasm
REACTOR_ADAPTER_SHA=6d52112df28f0caee9018f911f279f94f848230b2f0ed02138290363e925671f

# fixture sources that need the reference toolchain
SQLITE_SOURCES="$OUT/sqlite"

fetch() { # url sha dest
  local url=$1 sha=$2 dest=$3
  [ -f "$dest" ] || curl -sSL -o "$dest" "$url"
  local got; got=$(sha256sum "$dest" | cut -d' ' -f1)
  [ "$got" = "$sha" ] || { echo "HASH MISMATCH $url: $got != $sha" >&2; return 1; }
}

need() { command -v "$1" >/dev/null || { echo "missing tool: $1 (use tools/wasi-toolchain.nix)" >&2; exit 2; }; }

record() { # path
  ( cd "$ROOT" && sha256sum "$1" ) >> "$CACHE/CHECKSUMS.new"
}

# ---------------------------------------------------------------- SQLite
build_sqlite() {
  local src="$CACHE/sqlite-amalgamation-3540100"
  fetch "$SQLITE_URL" "$SQLITE_SHA" "$CACHE/sqlite-amalgamation-3540100.zip"
  [ -d "$src" ] || ( cd "$CACHE" && unzip -q sqlite-amalgamation-3540100.zip )
  # The shell's interactive prompt asks gethostname(); WASI has none. The
  # port source supplies ONLY that failed query (-1/ENOSYS), so the shell's
  # own documented "?" fallback runs: no fake hostname, no SQL/VFS change.
  zig cc -target wasm32-wasi -O2 \
    -DSQLITE_NOHAVE_SYSTEM \
    -D_WASI_EMULATED_PROCESS_CLOCKS -D_WASI_EMULATED_GETPID -D_WASI_EMULATED_SIGNAL \
    -Dgethostname=wmd_shell_gethostname \
    -include "$SQLITE_SOURCES/sqlite3-wasi-port.h" \
    -o "$OUT/sqlite3-p1.wasm" \
    "$src/shell.c" "$src/sqlite3.c" "$SQLITE_SOURCES/sqlite3-wasi-port.c" \
    -lm -lwasi-emulated-process-clocks -lwasi-emulated-getpid -lwasi-emulated-signal
  wasm-tools component new "$OUT/sqlite3-p1.wasm" \
    --adapt "$CACHE/$CMD_ADAPTER" -o "$OUT/sqlite3-p2.wasm"
  record tests/fixtures/wasi/sqlite3-p1.wasm
  record tests/fixtures/wasi/sqlite3-p2.wasm
}

# ---------------------------------------------------------------- main
main() {
  rm -f "$CACHE/CHECKSUMS.new"
  fetch "$ADAPTER_BASE/$CMD_ADAPTER" "$CMD_ADAPTER_SHA" "$CACHE/$CMD_ADAPTER"
  fetch "$ADAPTER_BASE/$REACTOR_ADAPTER" "$REACTOR_ADAPTER_SHA" "$CACHE/$REACTOR_ADAPTER"
  if [ "$CHECK" = 1 ]; then
    ( cd "$ROOT" && sha256sum -c tests/fixtures/wasi/CHECKSUMS )
    exit 0
  fi
  need zig
  need wasm-tools
  build_sqlite
  sort -k2 "$CACHE/CHECKSUMS.new" > "$OUT/CHECKSUMS"
  cat "$OUT/CHECKSUMS"
}

main "$@"
