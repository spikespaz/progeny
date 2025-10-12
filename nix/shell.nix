{ mkShell, callPackage, rust-bin, cargo-expand }:
let
  rust-stable = rust-bin.stable.latest.minimal.override {
    extensions = [ "rust-src" "rust-docs" "clippy" ];
  };
  rust-nightly-tools = callPackage ./rust-nightly-tools.nix {
    components = [ "rustfmt" "rust-analyzer" ];
  };
in mkShell {
  strictDeps = true;
  packages = [ rust-stable rust-nightly-tools cargo-expand ];
}
