{
  description = "cabal2nixWithoutIFD";

  inputs.purenix.url = "github:purenix-org/purenix";
  inputs.nixpkgs.follows = "purenix/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, purenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
      in
      {
        devShell = purenix.devShells.${system}.use-purenix;
      }
    );
}
