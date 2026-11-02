#!/usr/bin/env bash
# Build curl as a WASI 0.3 component with wasi-sdk 34 (development tool).
# Installs into the git-ignored .cache/; the runtime is unchanged.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLS="$ROOT/.cache/tools"
SDK_VERSION=34.0
CURL_VERSION=8.22.0
ARCH="$(uname -m)"
case "$ARCH" in
  aarch64|arm64) SDK_ARCH=arm64 ;;
  x86_64|amd64) SDK_ARCH=x86_64 ;;
  *) echo "unsupported architecture: $ARCH" >&2; exit 2 ;;
esac
SDK="$TOOLS/wasi-sdk-$SDK_VERSION-$SDK_ARCH-linux"
SRC="$TOOLS/curl-$CURL_VERSION"
BUILD="$SRC/build-wasi"
mkdir -p "$TOOLS"

if [ ! -x "$SDK/bin/clang" ]; then
  TAR="$TOOLS/wasi-sdk-$SDK_VERSION-$SDK_ARCH-linux.tar.gz"
  [ -f "$TAR" ] || curl -fsSL -o "$TAR" \
    "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-34/wasi-sdk-$SDK_VERSION-$SDK_ARCH-linux.tar.gz"
  tar -xf "$TAR" -C "$TOOLS"
fi
if [ ! -f "$SRC/configure" ]; then
  TAR="$TOOLS/curl-$CURL_VERSION.tar.gz"
  [ -f "$TAR" ] || curl -fsSL -o "$TAR" "https://curl.se/download/curl-$CURL_VERSION.tar.gz"
  tar -xf "$TAR" -C "$TOOLS"
fi

# wasi-sdk's setjmp.h requires the exception-handling proposal; curl only
# needs setjmp for its optional SIGALRM resolver timeout, which wasi has no
# signals for. Patch the unconditional include (config has HAVE_SIGSETJMP
# undefined).
python3 - "$SRC/lib/vdns/hostip.c" <<'PY'
import pathlib, sys
p = pathlib.Path(sys.argv[1]); s = p.read_text()
old = "#include <setjmp.h>  /* for sigjmp_buf, sigsetjmp() */\n#include <signal.h>"
new = "#ifdef HAVE_SIGSETJMP\n#include <setjmp.h>  /* for sigjmp_buf, sigsetjmp() */\n#endif\n#include <signal.h>"
if old in s:
    p.write_text(s.replace(old, new))
PY

mkdir -p "$BUILD"
if [ ! -x "$BUILD/src/curl" ]; then
  cd "$BUILD"
  CC="$SDK/bin/clang" AR="$SDK/bin/llvm-ar" RANLIB="$SDK/bin/llvm-ranlib" \
  CFLAGS="--target=wasm32-wasip3 -O2 -D_WASI_EMULATED_SIGNAL" \
  LDFLAGS="--target=wasm32-wasip3" LIBS="-lwasi-emulated-signal" \
  ../configure --host=wasm32-wasip3 --disable-shared --enable-static \
    --without-ssl --without-zlib --without-brotli --without-zstd \
    --without-libidn2 --without-libpsl --disable-ldap --disable-ldaps \
    --disable-rtsp --disable-dict --disable-telnet --disable-tftp \
    --disable-pop3 --disable-imap --disable-smb --disable-gopher \
    --disable-mqtt --disable-manual --disable-threaded-resolver \
    --disable-ntlm >/dev/null
  make -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)" >/dev/null
fi
"$SDK/bin/clang" --version | head -1
"$BUILD/src/curl" >/dev/null 2>&1 || true
ls -lh "$BUILD/src/curl"
cat <<MSG

curl WASI component ready:
  $BUILD/src/curl
  make test-curl-wasm
MSG
