# This file is used for testing the transpiled PureScript code.
# Check the documentation below on each overlay to see exactly what attributes
# each overlay defines, and how to build them.
#
# Before using this file, you must build the PureScript library
# `./purescript-cabal-parser`.  See the README.md for instructions on how to do
# this.

let

  flake-lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = flake-lock.nodes.nixpkgs.locked.narHash;
  };

  # This is the compiled output of the
  # `./purescript-cabal-parser/src/Main.purs` file.  Note that PureNix takes
  # PureScript files and transpiles them to Nix files as a record set, with
  # each attribute name being a PureScript function name.  See the call to
  # `rawCabalFileToPkgDef` in `haskellPkgDrv` below for an example of using
  # a function from `purenixMain`.
  #
  # Try importing this `./purescript-cabal-parser/output/Main/default.nix` file
  # in the Nix repl and playing around with it.
  #
  # It feels pretty neat being able to call functions written in PureScript
  # from Nix.
  purenix-Main = import ./purescript-cabal-parser/output/Main;

  overlays = [
    # This overlay adds a top-level package called `exampleHaskellPackageFromNix`.
    #
    # This top-level Haskell package is built by parsing the
    # `example-haskell-package.cabal` file using the PureScript function
    # `rawCabalFileToPkgDef`.
    #
    # This overlay is similar to the `exampleNixpkgsOverlay`, except it is
    # defined in Nix instead of PureScript.  Check the
    # `./purescript-cabal-parser/src/Main.purs` file to compare how the
    # `exampleNixpkgsOverlay` function is written in PureScript.
    #
    # You can build `exampleHakellPackageFromNix` like the following:
    #
    # ```
    # $ nix-build ./test.nix -A exampleHaskellPackageFromNix
    # /nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
    # ```
    (final: prev: {
      exampleHaskellPackageFromNix =
        let
          # `rawCabalFile` is just a string that contains the raw contents of the
          # cabal file for our example-cabal-library Haskell package.
          rawCabalFile =
            builtins.readFile ./example-cabal-library/example-cabal-library.cabal;

          # This uses the PureScript function `rawCabalFileToPkgDef` here from Nix.
          # This function parses a raw Cabal file, and turns it into something
          # that you can pass to `haskellPackages.callPackage`.
          #
          # `haskellPkgDrv` will become a function similar to this:
          #
          # ```
          # { mkDerivation, aeson, base, lib }:
          # mkDerivation {
          #   pname = "example-cabal-library";
          #   version = "0.1.0.0";
          #   src = ./example-cabal-library;
          #   isLibrary = false;
          #   isExecutable = true;
          #   executableHaskellDepends = [ aeson base ];
          #   license = lib.licenses.bsd3;
          # }
          # ```
          haskellPkgDrv =
            purenix-Main.rawCabalFileToPkgDef rawCabalFile ./example-cabal-library;
        in
        final.haskellPackages.callPackage haskellPkgDrv {};
    })

    # This overlay adds a top-level package called `exampleHaskellPackage`.
    #
    # This is exactly the same as the previous overlay, except it is defined in
    # PureScript. Check `./purescript-cabal-parser/src/Main.purs` to see how
    # this overlay is defined.
    #
    # You can build `exampleHakellPackage` like the following:
    #
    # ```
    # $ nix-build ./test.nix -A exampleHaskellPackage
    # /nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
    # ```
    purenix-Main.exampleNixpkgsOverlay

    # This overlay is similar to `exampleNixpkgsOverlay`, but instead of
    # adding a top-level package, it adds a Haskell package to `haskellPackages`.
    # Adding a Haskell package is significantly more involved than adding a
    # top-level package.  Check out the source in
    # `./purescript-cabal-parser/src/Main.purs` to see how this is defined.
    #
    # You can build this Haskell package like the following:
    #
    # ```
    # $ nix-build ./test.nix -A haskellPackages.exampleHaskellPackage
    # /nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
    # ```
    purenix-Main.exampleNixpkgsHaskellOverlay

    # This overlay is almost the exact same as `exampleNixpkgsHaskellOverlay`,
    # but the PureScript code defining it is strongly typed.
    # `exampleNixpkgsHaskellOverlay` uses mostly dynamically-typed Nix values
    # like `AttrSet` and unsafe getters.  But `exampleNixpkgsHaskellOverlayTyped`
    # uses realistic types for each attribute set.  This is possible because of
    # PureScripts row types.
    #
    # See `./purescript-cabal-parser/src/Main.purs` for all the gory details.
    #
    # You can build the Haskell package added in this overlay like the following:
    #
    # ```
    # $ nix-build ./test.nix -A haskellPackages.exampleHaskellPackageTyped
    # /nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
    # ```
    purenix-Main.exampleNixpkgsHaskellOverlayTyped

    # This is an example overlay that shows how the normal `callCabal2nix`
    # function is used to build a Haskell package.
    #
    # This is useful because you can tell Nix to disallow IFD and confirm
    # this call fails:
    #
    # ```
    # $ nix-build --option allow-import-from-derivation false ./test.nix -A exampleHaskellPackageWithIFD
    # error: cannot build '/nix/store/q5hq65ynrqnnnjs3kl1ggsx2pkwlw702-cabal2nix-example-cabal-library.drv'
    # during evaluation because the option 'allow-import-from-derivation' is disabled
    # ```
    #
    # Now, you can go back and try building all the above packages with
    # `allow-import-from-derivation` disabled.  You'll see that they all
    # still successfully build, because they are not using IFD.  Instead,
    # the `.cabal` file is parsed using the Nix code produced from PureNix.
    (final: prev: {
      exampleHaskellPackageWithIFD =
        final.haskellPackages.callCabal2nix "example-cabal-library" ./example-cabal-library { };
    })
  ];

  pkgs = import nixpkgs-src { inherit overlays; };

in

pkgs
