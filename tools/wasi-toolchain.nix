# Development-only toolchain for the WASI fixture builder.
#
# Not part of the production runtime: wasmdown itself has zero mandatory
# third-party dependencies; this flake pins the compilers that BUILD the
# test fixtures (PLAN.md §10.3, §12.2). The pinned revisions are the ones
# recorded for the fixture experiment:
#
#   zig          0.17.0
#   wasm-tools   1.262.0
#   wit-bindgen  0.63.0
#   wasmtime     50.0.0     (comparison executor only)
#
# Adapter releases are pinned separately (v50.0.1 command/reactor assets in
# specs/lock.toml) and fetched by tools/build-wasi-fixtures.sh.
{
  description = "wasmdown WASI fixture toolchain (development only)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/885ba4d038f10c3d17d7a92e8c3cc185b99c9158";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-linux" "x86_64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = forAll (pkgs: {
        default = pkgs.buildEnv {
          name = "wasmdown-wasi-toolchain";
          paths = [
            pkgs.zig
            pkgs.wasm-tools
            pkgs.wit-bindgen
            pkgs.wasmtime
            pkgs.python3
            pkgs.curl
            pkgs.unzip
          ];
          # assert the pinned versions we recorded
          postBuild = ''
            test "$(zig version)" = "0.17.0"
            test "$(wasm-tools --version | awk '{print $2}')" = "1.262.0"
            test "$(wit-bindgen --version | awk '{print $2}')" = "0.63.0"
            test "$(wasmtime --version | awk '{print $2}')" = "50.0.0"
          '';
        };
      });
    };
}
