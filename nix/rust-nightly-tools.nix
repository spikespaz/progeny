{ lib, symlinkJoin, rust-bin, components }:
let
  toolchain = rust-bin.selectLatestNightlyWith
    (toolchain: toolchain.minimal.override { extensions = components; });
  componentAttrs = lib.filterAttrs (name: _: lib.elem name components)
    toolchain.availableComponents;
in symlinkJoin {
  pname = "rust-tools";
  inherit (toolchain) version;
  paths = lib.attrValues componentAttrs;
  passthru = componentAttrs;
}
