
let

  flake-lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = flake-lock.nodes.nixpkgs.locked.narHash;
  };

  purenix-Main = import ./purescript-cabal-parser/output/Main;

  overlays = [
    (final: prev: {
      exampleHaskellPackageFromNix =
        let
          rawCabalFile =
            builtins.readFile ./example-cabal-library/example-cabal-library.cabal;

          purenixMain = import ./purescript-cabal-parser/output/Main;

          haskellPkgDrv =
            purenixMain.rawCabalFileToPkgDef rawCabalFile ./example-cabal-library;
        in
        final.haskellPackages.callPackage haskellPkgDrv {};
    })

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
