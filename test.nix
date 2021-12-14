
let

  flake-lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = flake-lock.nodes.nixpkgs.locked.narHash;
  };

  purenix-Main = import ./purescript-cabal-parser/output/Main;

  overlays = [
    purenix-Main.exampleNixpkgsOverlay
    purenix-Main.exampleNixpkgsHaskellOverlay
    purenix-Main.exampleNixpkgsHaskellOverlayTyped
  ];

  pkgs = import nixpkgs-src { inherit overlays; };

in

pkgs
