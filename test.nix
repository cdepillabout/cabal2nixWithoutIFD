
let

  flake-lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = flake-lock.nodes.nixpkgs.locked.narHash;
  };

  purenix-Main = import ./purescript-cabal-parser/output/Main;

  overlays = [
    # TODO: Write a few comments about what these overlays contain.
    purenix-Main.exampleNixpkgsOverlay

    purenix-Main.exampleNixpkgsHaskellOverlay

    purenix-Main.exampleNixpkgsHaskellOverlayTyped

    (final: prev: {
      exampleHaskellPackageWithIFD =
        final.haskellPackages.callCabal2nix "example-cabal-library" ./example-cabal-library { };
    })
  ];

  pkgs = import nixpkgs-src { inherit overlays; };

in

pkgs
