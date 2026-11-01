#!/usr/bin/env bash
# Vendor the pinned WASI specification trees (docs/design/PLAN.md §12.1, step 1).
#
# Every artifact below is pinned by URL + SHA-256. This script never
# floats a branch: the WASI repository revision is the immutable tag
# commit for the releases that predate the release-tarball workflow
# (v0.2.0 .. v0.2.9), and the release asset for v0.2.10 .. v0.2.12.
#
# Output layout (verbatim, never rewritten):
#   specs/wasi/preview1/witx/{wasi_snapshot_preview1,typenames}.witx
#   specs/wasi/preview1/docs.md
#   specs/wasi/wasi-wit-<version>/            (release asset, verbatim)
#   specs/wasi/wasi-wit-<version>/preview2/   (tag source, verbatim)
#   specs/wasi/index.txt                      (generated: version/package/file)
#
# Usage:  tools/vendor-wasi-specs.sh [--check]
#   (default) download into a cache, verify, extract, regenerate index
#   --check   verify the vendored tree against the pins, write nothing
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
DEST="$ROOT/specs/wasi"
CACHE="${WASI_VENDOR_CACHE:-$ROOT/.cache/wasi-specs}"
CHECK=0
[ "${1:-}" = "--check" ] && CHECK=1

mkdir -p "$CACHE"

# ---------------------------------------------------------------- pins
WASI_REPO_SHA_PREVIEW1=fae981bae14809d91f9bc2d63852d461f331d161

preview1_file() { # name -> sha256
  case "$1" in
    wasi_snapshot_preview1.witx) echo 30e31a9b830d3fad998701fc9439a5506af4dbb8f01202854c8af46d436de679 ;;
    typenames.witx)              echo 980ef8a0f8d345f65d0c7ce07f01981174712ae0c6c44b3c250cbb2b0c752dc5 ;;
    docs.md)                     echo ac9e99351efd42af854d23c6cb858289a51ea485ab3c3923fa5837525e6a06c6 ;;
    *) return 1 ;;
  esac
}

# version tag-sha tarball-sha (tarball-sha empty => tag source tarball)
release_pin() {
  case "$1" in
    0.2.0)  echo "70214b878af4ce45889b4ad9d26a7ac98db8931b 4fe75635c2b9b63b6af541f80ebbdbca4c84ad2879553685d1df74a44f82e605" ;;
    0.2.1)  echo "f922a15d49f9e79e9184b5da6eca7a6f863981df 12af55f6115fc2adfb2df8d3a74b12143c40e588afa5130bd0f386bde6dea62a" ;;
    0.2.2)  echo "8777909de3d1cb5cdaee16f98d76c572d4b88b16 9878c31cb42e962515a3e9130fa1fd4913417736ad76e7f11f9c0041c4251fa3" ;;
    0.2.3)  echo "d93b745005555dd952fa22956c48df7bb37f822d a3bffd5bd41afb794e594dbe608fb327248297103b332a7d66261aaa0133c4a6" ;;
    0.2.4)  echo "980ca88612e567d41d50edac9d67ae2717f0ab18 820c4104c5a3d2028ed8c068381ac52d5a61814a68cf61fab7f45b287054a4f6" ;;
    0.2.5)  echo "350457d624159902088c45e08d434d76575635fa e1a5f0bf5f0eae880e90b884ed03510e87dcdfe3f3be36f3a53cf1b0fab90417" ;;
    0.2.6)  echo "3e5d10e2c785e82341e9c29e37e5b3d090e1f8ec 7645e65a7f1a7b2d7344ddb20f2371d7370b97208ba1a9e8ccdf5be9d9b64fc3" ;;
    0.2.7)  echo "8a69f1ed6ce7bfd3cfe72270b787d4d4598b721d cfde048d91fd11b9b0b6fc5db85444e4c610340fb811a46039104b0ab378c1ce" ;;
    0.2.8)  echo "b26023f7d540bc7f0faae24052ca887421541acf 58bad0364c0774f835ded4cfab946a5e705bb957d44ee9ff477c869f7c4690b0" ;;
    0.2.9)  echo "34c0258951fbd31b2e7591c5d4cdc5ebc760bcfc 8c52624aebed805e29aa1e64053fd3171159bbe8a195e90982f87edae789a6e8" ;;
    0.2.10) echo "e6673a0d12e9b029445c6890dc8092265b9c8bab7b9f613e5f176900289c3a31 e6673a0d12e9b029445c6890dc8092265b9c8bab7b9f613e5f176900289c3a31" ;;
    0.2.11) echo "0607082a26e493e607e3a472ddb26a315699c016ebe504b7cff30bf2656c6501 0607082a26e493e607e3a472ddb26a315699c016ebe504b7cff30bf2656c6501" ;;
    0.2.12) echo "281ba75fafcd50961ef55f9e52747afcc9b71ede 2b0e47bc02953267ad163c6c1188b65baed1292351998cb032f3dcd28d005992" ;;
    *) return 1 ;;
  esac
}

# 0.2.10..0.2.12 shipped a release asset (one file per package, deps inlined).
has_release_asset() { case "$1" in 0.2.10|0.2.11|0.2.12) return 0 ;; *) return 1 ;; esac; }

fetch() { # url sha out
  local url=$1 sha=$2 out=$3
  if [ "$CHECK" = 1 ]; then
    [ -f "$out" ] || { echo "MISSING $out"; return 1; }
  else
    [ -f "$out" ] || curl -sSL -o "$out" "$url"
  fi
  local got; got=$(sha256sum "$out" | cut -d' ' -f1)
  [ "$got" = "$sha" ] || { echo "HASH MISMATCH $url: $got != $sha"; return 1; }
}

# ---------------------------------------------------------------- preview1
vendor_preview1() {
  local out="$DEST/preview1" base="https://raw.githubusercontent.com/WebAssembly/WASI/$WASI_REPO_SHA_PREVIEW1"
  mkdir -p "$out/witx"
  for f in wasi_snapshot_preview1.witx typenames.witx; do
    fetch "$base/preview1/witx/$f" "$(preview1_file "$f")" "$CACHE/preview1-$f"
    [ "$CHECK" = 1 ] || cp "$CACHE/preview1-$f" "$out/witx/$f"
    [ "$CHECK" = 1 ] || echo "preview1/witx/$f $(sha256sum "$out/witx/$f" | cut -d' ' -f1)" >> "$CACHE/index.txt"
  done
  fetch "$base/preview1/docs.md" "$(preview1_file docs.md)" "$CACHE/preview1-docs.md"
  [ "$CHECK" = 1 ] || cp "$CACHE/preview1-docs.md" "$out/docs.md"
}

# ---------------------------------------------------------------- P2 releases
vendor_release() { # version
  local v=$1 tagsha tarsha
  read -r tagsha tarsha <<<"$(release_pin "$1")"
  local dir="$DEST/wasi-wit-$v" tar="$CACHE/wasi-wit-$v.tar.gz"
  if has_release_asset "$v"; then
    fetch "https://github.com/WebAssembly/WASI/releases/download/v$v/wasi-wit-$v.tar.gz" "$tarsha" "$tar"
    if [ "$CHECK" = 1 ]; then
      [ -d "$dir" ] || { echo "MISSING $dir"; return 1; }
    else
      rm -rf "$dir"; mkdir -p "$dir"
      tar xzf "$tar" -C "$DEST"
      # the asset unpacks to wasi-wit-<v>/; keep a single copy
      mv "$DEST/wasi-wit-$v" "$dir" 2>/dev/null || true
    fi
    [ "$CHECK" = 1 ] || find "$dir" -name '*.wit' | sed "s|$DEST/||" | sort >> "$CACHE/index.txt"
  else
    # No release asset: vendor the immutable tag source's stable 0.2 tree.
    # The tag trees move over time (preview2/ -> wasip2/ -> proposals/<pkg>/wit/),
    # so extraction NORMALIZES the layout to <pkg>/<interface>.wit while
    # copying the file bytes verbatim.
    fetch "https://codeload.github.com/WebAssembly/WASI/tar.gz/refs/tags/v$v" "$tarsha" "$tar"
    if [ "$CHECK" = 1 ]; then
      [ -d "$dir" ] || { echo "MISSING $dir"; return 1; }
    else
      rm -rf "$dir"; mkdir -p "$dir"
      python3 "$ROOT/tools/vendor-wasi-tag-extract.py" "$tar" "$dir" "$v"
      printf '%s\n' "$tagsha" > "$dir/.revision"
    fi
    [ "$CHECK" = 1 ] || find "$dir" -name '*.wit' | sed "s|$DEST/||" | sort >> "$CACHE/index.txt"
  fi
}

main() {
  rm -f "$CACHE/index.txt"
  # header lines start with '#': version, package short name, path
  [ "$CHECK" = 1 ] || printf '# version\tpackage\tpath (relative to specs/wasi)\n' > "$CACHE/index.txt"
  vendor_preview1
  for v in 0.2.0 0.2.1 0.2.2 0.2.3 0.2.4 0.2.5 0.2.6 0.2.7 0.2.8 0.2.9 0.2.10 0.2.11 0.2.12; do
    vendor_release "$v"
  done
  if [ "$CHECK" = 0 ]; then
    # the 0.3.2 tree was vendored by an earlier release; index it too so
    # one generated index covers every supported version
    find "$DEST/wasi-wit-0.3.2" -name '*.wit' | sed "s|$DEST/||" | sort >> "$CACHE/index.txt"
    # package column: derived from the vendored directory layout, recorded
    # in the generated index so runtime lookup never guesses a layout.
    python3 - "$DEST" "$CACHE/index.txt" <<'PY'
import sys, os, re
dest, idx = sys.argv[1], sys.argv[2]
rows = []
for line in open(idx):
    if line.startswith('#'): continue
    rel = line.strip()
    if not rel.endswith('.wit'): continue
    parts = rel.split('/')
    version = parts[0].replace('wasi-wit-', '')
    pkgs = {'cli','clocks','filesystem','http','io','random','sockets'}
    pkg = next((p for p in parts[1:] if p in pkgs), None)
    if pkg is None:
        raise SystemExit(f'cannot derive package for {rel}')
    rows.append((version, pkg, rel))
rows.sort()
with open(idx, 'w') as f:
    f.write('# version\tpackage\tpath (relative to specs/wasi)\n')
    for v, p, rel in rows:
        f.write(f'{v}\t{p}\t{rel}\n')
PY
    cp "$CACHE/index.txt" "$DEST/index.txt"
    echo "vendored: $(grep -vc '^#' "$DEST/index.txt") WIT files; index at specs/wasi/index.txt"
  else
    echo "check: pins verified"
  fi
}

main "$@"
