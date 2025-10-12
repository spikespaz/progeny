{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems = {
      url = "github:nix-systems/default";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, rust-overlay, systems, }:
    let
      inherit (nixpkgs) lib;
      eachSystem = lib.genAttrs (import systems);
      pkgsFor = eachSystem (system:
        import nixpkgs {
          localSystem = system;
          overlays = [ rust-overlay.overlays.default ];
        });

      rustNightlyTools = pkgs: components:
        pkgs.callPackage ./nix/rust-nightly-tools.nix { inherit components; };
    in {
      devShells = lib.mapAttrs
        (system: pkgs: { default = pkgs.callPackage ./nix/shell.nix { }; })
        pkgsFor;

      formatter = lib.mapAttrs (system: pkgs:
        let rustfmt = (rustNightlyTools pkgs [ "rustfmt" ]).rustfmt;
        in pkgs.callPackage ./nix/formatter.nix {
          inherit rustfmt;
          nixfmt = pkgs.nixfmt-classic;
        }) pkgsFor;
    };
}
