# cabal2nixWithoutIFD

This repo contains a proof-of-concept for a
[`callCabal2nix`](https://github.com/NixOS/nixpkgs/blob/d263feb6f6392939c4c5e0a2608a450f65417d18/pkgs/development/haskell-modules/make-package-set.nix#L220)
function written in Nix. `callCabal2nix` uses
[Import From Derivation (IFD)](https://blog.hercules-ci.com/2019/08/30/native-support-for-import-for-derivation/)
and a Haskell program [cabal2nix](https://github.com/NixOS/cabal2nix) to take
a Haskell `.cabal` file (like
[`example-cabal-library.cabal`](`./example-cabal-library/example-cabal-library.cabal`))
and translate it into a derivation that can be built with Nix.

One unfortunate part of `callCabal2nix` is that `cabal2nix` is written in
Haskell, so IFD must be used to build the resulting derivation.  IFD can often
be slower than just doing things with native Nix, and it is not allowed
in Nixpkgs.

The `callCabal2nixWithoutIFD` function provided by this repo is written in Nix,
so it doesn't have either of the above downsides.  Cabal files are parsed in
Nix directly, so IFD is not needed.

Internally, this `callCabal2nixWithoutIFD` function is written in PureScript
and transpiled to Nix using [PureNix](https://github.com/purenix-org/purenix).
Writing a parser for a complicated format like a Cabal file is much more
reasonable in a language like PureScript than Nix itself.  While this repo
is just a proof-of-concept, this is an approach that could potentially be
expanded to write a full parser for `.cabal` files.

## Running

Compiling `purescript-cabal-parser` to Nix can be done with the following
steps.

First, get into a Nix devShell:

```console
$ nix develop
```

The, change to `./purescript-cabal-parser` directory and run `spago build`:

```console
$ cd ./purescript-cabal-parser
$ spago build --verbose
```

`spago build` does the following things:

1.  Looks for all `.purs` files in `./purescript-cabal-parser/src/`.

    The location to search for `.purs` files can be changed in
    `./purescript-cabal-parser/spago.dhall`.

2.  Compile the `.purs` files to corefn and output in
    `./purescript-cabal-parser/output/`.

    For instance, `./purescript-cabal-parser/src/Main.purs` will be compiled to
    `./purescript-cabal-parser/output/Main/corefn.json`.

3.  `spago` will try to run the `backend` command defined in
    `./purescript-cabal-parser/spago.dhall`.

    This is set to `cd ../purenix && cabal run purenix`, so the `purenix`
    executable will be built and run.

4.  `purenix` needs to look for all `corefn.json` files in
    `./purescript-cabal-parser/output/`, translate them to JSON, and then
    output them somewhere.

    It also needs to take into account the optional FFI files for each PureScript module.

    For instance, if `./purescript-cabal-parser/src/Main.purs` defines FFI
    functions, there needs to be a corresponding
    `./purescript-cabal-parser/src/Main.nix` file.

    Note that `spago build` does not copy this
    `./purescript-cabal-parser/src/Main.nix` file to
    `./purescript-cabal-parser/output/` for us, so we need to explicitly look
    for `./purescript-cabal-parser/src/Main.nix` in `purenix`.  I haven't yet
    looked at how other PureScript backends handle this.
