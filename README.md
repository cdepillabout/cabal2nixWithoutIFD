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

This repo provides a `callCabal2nixWithoutIFD` function that is written in Nix,
so it doesn't have either of the above downsides.  Cabal files are parsed in
Nix directly, so IFD is not needed.

Internally, this `callCabal2nixWithoutIFD` function is written in PureScript
and transpiled to Nix using [PureNix](https://github.com/purenix-org/purenix).
Writing a parser for a complicated format like a Cabal file is much more
reasonable in a language like PureScript than Nix itself.

While this repo is just a proof-of-concept, this approach of writing a
complicated parser in PureNix could reasonably be extended to write a full
parser for `.cabal` files.  Writing a similar parser directly in raw Nix would
be considerably more difficult.

## Compiling PureScript Code to Nix

Compiling `purescript-cabal-parser` to Nix can be done with the following
steps.

First, get into a Nix devShell:

```console
$ nix develop
```

The, change to `./purescript-cabal-parser` directory and run `spago build`:

```console
$ cd ./purescript-cabal-parser/
$ spago build
```

This transpiles the PureScript code to Nix.  The transpiled Nix code is placed
in `./purescript-cabal-parser/output/`.

## Running Transpiled PureScript Code

The transpiled PureScript code can be tested by using the [`test.nix`](./test.nix)
script.

Here's a derivation for a Haskell package that was built using
`callCabal2nixWithoutIFD`:

```console
$ nix-build ./test.nix -A exampleHaskellPackage
...
/nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
```

Here's a derivation for a Haskell package that was built using
the normal `callCabal2nix`:

```console
$ nix-build ./test.nix -A exampleHaskellPackageWithIFD
...
/nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
```

You can disable IFD in Nix and see that the Haskell derivation built with
`callCabal2nixWithoutIFD` still builds successfully:

```console
$ nix-build --option allow-import-from-derivation false ./test.nix -A exampleHaskellPackage
...
/nix/store/xb0yxkpzdz3zjqmcxwa9z6r6r98yfizz-example-cabal-library-0.1.0.0
$ nix-build --option allow-import-from-derivation false ./test.nix -A exampleHaskellPackageWithIFD
error: cannot build '/nix/store/q5hq65ynrqnnnjs3kl1ggsx2pkwlw702-cabal2nix-example-cabal-library.drv'
during evaluation because the option 'allow-import-from-derivation' is disabled
```

I recommend reading through the [`test.nix`](./test.nix) file to see what other
derivations have been defined that you can try building.  The file is heavily
commented.

## Playing around in the Nix REPL

You may be interested in taking a look at the PureScript code that defines the
above derivations.  Most of the interesting code is in
[`purescript-cabal-parser/src/Main.purs`](./purescript-cabal-parser/src/Main.purs).

You can load the transpiled PureScript code in a Nix REPL to play around with
it:

```console
$ cd ./purescript-cabal-parser/
$ nix repl ./purescript-cabal-parser/output/Main
```

This puts you in a Nix REPL with all the functions and datatypes from the
[`Main`](./purescript-cabal-parser/src/Main.purs) module available.

For instance, `Main` defines a function called `cabalParser`:

```purescript
cabalParser :: String -> Either (Array String) CabalFile
```

This takes the raw text of a Cabal file and parses it into a `CabalFile`
datatype:

```purescript
type CabalFile =
  { name :: String
  , version :: String
  , license :: License
  , executable :: Executable
  }

data License = LicenseBSD3

type Executable =
  { name :: String
  , buildDepends :: Array String
  }
```

Let's try calling `cabalParser` from the Nix REPL:

```nix
nix-repl> rawCabalFile = builtins.readFile ./
nix-repl> :p cabalParser rawCabalFile
{ __field0 = { executable = { buildDepends = [ "base" "aeson" ]; name = "example-cabal-library"; }; license = { __tag = "LicenseBSD3"; }; name = "example-cabal-library"; version = "0.1.0.0"; }; __tag = "Right"; }
```

If you squint, you can roughly see this attrset corresponds to the `CabalFile`
datatype.

There are lots of comments in the
[`Main.purs`](./purescript-cabal-parser/src/Main.purs) file, so you may want to
skim through it to see what else is available.

## Caveats

The cabal parser implemented in `./purescript-cabal-parser/` is completely a
proof-of-concept.  It only parses the absolute simplest of `.cabal` files.
I wouldn't recommend using it any arbitrary Haskell package.

However, I hope the PureScript code in this repository shows the potential of
PureNix for writing things that would be difficult in raw Nix.
