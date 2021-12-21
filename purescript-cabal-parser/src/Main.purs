module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (findMap, many, some, (:))
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import NixBuiltins (AttrSet, Derivation, Path, abort, appendPath, concatChars, getAttr, getAttrFromPath, readFile, toAttrSet, trace, (!.), (//!))
import Parsec (Parser, alphaNums, char, count, eof, notChar, notChars, oneOf, optional, runParser, sepBy1, space, string)
import Unsafe.Coerce (unsafeCoerce)

-- This is useful for "type-driven design", but shouldn't be used in an actual
-- function.
-- undefined :: forall a. a
-- undefined = unsafeCoerce unit

-- | This represents an `executable` section in a Cabal file.
type Executable =
  { name :: String
  , buildDepends :: Array String
  }

-- | A datatype representing the `license` in a Cabal file.
-- | This should be expanded to cover more licenses.
data License = LicenseBSD3

derive instance genericLicense :: Generic License _

instance showLicense :: Show License where
  show = genericShow

-- | This represents a parsed Cabal file.  Note that for now
-- | we only parse the fields that are required for the Nix derivation.
-- | This includes things like `buildDepends`, but not things like
-- | `default-language`.
type CabalFile =
  { name :: String
  , version :: String
  , license :: License
  , executable :: Executable
  }

type IndentAmount = Int

-- | This represents a raw property in a `.cabal` file.
-- | For instance, Cabal files have a `name` property.
-- | This `name` property would be created as a `SimpleRawProp`:
-- |
-- | ```purescript
-- | SimpleRawProp "name" "example-cabal-library"
-- | ```
-- |
-- | A `RecursiveRawProp` is used for something like an `executable` section.
data RawProp = SimpleRawProp String String | RecursiveRawProp String String (Array RawProp)

derive instance genericRawProp :: Generic RawProp _

instance showRawProp :: Show RawProp where
  show rawProp = genericShow rawProp

-- | A `RawCabalFile` is an array of `RawProp`s.
data RawCabalFile = RawCabalFile (Array RawProp)

derive instance genericRawCabalFile :: Generic RawCabalFile _

instance showRawCabalFile :: Show RawCabalFile where
  show = genericShow

parseRawPropKey :: Parser String
parseRawPropKey = map concatChars (some (notChar ':'))

parseRawPropVal :: Parser String
parseRawPropVal = do
  val <- map concatChars (some (notChar '\n'))
  char '\n'
  pure val

parseSimpleRawProp :: Parser RawProp
parseSimpleRawProp = do
  key <- parseRawPropKey
  char ':'
  void $ some space
  val <- parseRawPropVal
  pure $ SimpleRawProp key val

parseRecursiveRawPropKey :: Parser String
parseRecursiveRawPropKey = alphaNums

parseRecursiveRawProp :: IndentAmount -> Parser RawProp
parseRecursiveRawProp indentAmount = do
  key <- parseRecursiveRawPropKey
  void $ some space
  val <- parseRawPropVal
  recursiveProps <- parseRawProps (indentAmount + 2)
  pure $ RecursiveRawProp key val recursiveProps

parseRawProp :: IndentAmount -> Parser RawProp
parseRawProp indentAmount = do
  void $ many (char '\n')
  void $ count indentAmount space
  parseRecursiveRawProp indentAmount <|> parseSimpleRawProp

parseRawProps :: IndentAmount -> Parser (Array RawProp)
parseRawProps indentAmount = some (parseRawProp indentAmount)

parseRawCabalFile :: Parser RawCabalFile
parseRawCabalFile = do
  res <- map RawCabalFile (parseRawProps 0)
  eof
  pure res

-- | This is the Nix `Path` of the `example-cabal-library.cabal` file.
-- | This is convenient for being able to embed things like `rawCabalFileStr`,
-- | but this should really be taken as an argument everywhere that needs it.
foreign import cabalFilePath :: Path

rawCabalFileStr :: String
rawCabalFileStr = readFile cabalFilePath

licenseParser :: Parser License
licenseParser =
  oneOf [ string "BSD-3-Clause" $> LicenseBSD3 ]

buildDependParser :: Parser String
buildDependParser = do
  libName <- alphaNums
  void $ many (notChars [',', '\n'])
  pure libName

buildDependsParser :: Parser (Array String)
buildDependsParser =
  sepBy1 buildDependParser (char ',' *> optional space)

parsedRawCabalFile ::  String -> Either (Int /\ Array String) (Int /\ RawCabalFile)
parsedRawCabalFile rawCabalFileString = runParser rawCabalFileString parseRawCabalFile

getSimpleProp :: String -> Array RawProp -> Either (Array String) String
getSimpleProp key rawProps =
  case findMap pred rawProps of
    Nothing -> Left [ "could not find key " <> key ]
    Just val -> Right val
  where
    pred :: RawProp -> Maybe String
    pred =
      case _ of
        SimpleRawProp key' val | key' == key -> Just val
        _ -> Nothing

getSimplePropParse
  :: forall a. Parser a -> String -> Array RawProp -> Either (Array String) a
getSimplePropParse parser key rawProps = do
  rawVal <- getSimpleProp key rawProps
  case runParser rawVal parser of
    Left (_ /\ err) -> Left err
    Right (_ /\ parsedVal) -> Right parsedVal

getRecursiveProp
  :: String -> Array RawProp -> Either (Array String) (String /\ Array RawProp)
getRecursiveProp key rawProps = do
  case findMap pred rawProps of
    Nothing -> Left [ "could not find key " <> key ]
    Just r -> Right r
  where
    pred :: RawProp -> Maybe (String /\ Array RawProp)
    pred =
      case _ of
        RecursiveRawProp key' val inner | key' == key -> Just (val /\ inner)
        _ -> Nothing

cabalFileFromRaw :: RawCabalFile -> Either (Array String) CabalFile
cabalFileFromRaw (RawCabalFile rawProps) = do
  name <- getSimpleProp "name" rawProps
  version <- getSimpleProp "version" rawProps
  license <- getSimplePropParse licenseParser "license" rawProps
  executableName /\ executableProps <- getRecursiveProp "executable" rawProps
  buildDepends <- getSimplePropParse buildDependsParser "build-depends" executableProps
  pure
    { name
    , version
    , license
    , executable:
        { name: executableName
        , buildDepends
        }
    }

-- | Parse a raw `.cabal` file into a `CabalFile`.  This function takes a
-- | `String` of the text of a `.cabal` file.
-- |
-- | This function starts by parsing the raw `.cabal` file into key-val mapping
-- | `RawCabalFile`, and then parses all the required fields from this
-- | `RawCabalFile` into a `CabalFile`.
cabalParser :: String -> Either (Array String) CabalFile
cabalParser rawCabalFileString = do
  case parsedRawCabalFile rawCabalFileString of
    Left (_ /\ err) -> Left err
    Right (_ /\ rawCabalFile) -> cabalFileFromRaw rawCabalFile

licenseToAttrPath :: License -> Array String
licenseToAttrPath =
  case _ of
    LicenseBSD3 -> ["lib", "licenses", "bsd3"]

-- | This datatype represents a Nix function that takes an attribute set.
-- |
-- | For instance, imagine the following Nix function:
-- |
-- | ```nix
-- | { a, b }: a + b
-- | ```
-- |
-- | This could be constructed from PureScript with `mkFunctionWithArgs`:
-- |
-- | ```purescript
-- | mkFunctionWithArgs ["a", "b"] (\args -> (args !. "a") + (args !. "b"))
-- | ```
foreign import data FunctionWithArgs :: Type

foreign import haskellPackagePath :: Path

-- |  See the docs on `FunctionWithArgs`.
foreign import mkFunctionWithArgs
  :: forall a. Array String -> (AttrSet -> a) -> FunctionWithArgs

-- | Take an input `CabalFile` and convert it to a Nix function that is
-- | compatible with `callPackage`.
-- |
-- | The Nix function this produces will look similar to the following:
-- |
-- | ```nix
-- | { mkDerivation, aeson, base, lib }:
-- | mkDerivation {
-- |   pname = "example-cabal-library";
-- |   version = "0.1.0.0";
-- |   src = src;
-- |   isLibrary = false;
-- |   isExecutable = true;
-- |   executableHaskellDepends = [ aeson base ];
-- |   license = lib.licenses.bsd3;
-- | }
-- | ```
cabalFileToPkgDef :: CabalFile -> Path -> FunctionWithArgs
cabalFileToPkgDef { name, version, license, executable } src =
  mkFunctionWithArgs
    ("mkDerivation" : "lib" : executable.buildDepends)
    \args ->
      (getAttr "mkDerivation" args)
        { pname: name
        , version
        , src
        , isLibrary: false
        , isExecutable: true
        , executableHaskellDepends: map (\buildDepend -> getAttr buildDepend args) executable.buildDepends
        , license: getAttrFromPath (licenseToAttrPath license) args
        }


-- | Take a raw Cabal file as String, parse it, and turn it into a `FunctionWithArgs`.
-- | Call `cabalFileToPkgDef` with the `CabalFile` produced by
-- | `cabalParser`.  Throw an error if `cabalParser` returns `Left`.
-- |
-- | Check the documentation on `cabalFileToPkgDef` to see what
-- | the `FunctionWithArgs` this produces looks like.
-- |
-- | This can be built in the Nix repl like the following:
-- |
-- | ```nix
-- | nix-repl> haskellPackages.callPackage (rawCabalFileToPkgDef rawCabalFileStr haskellPackagePath)  {}
-- | «derivation /nix/store/p8ws0vwn26rhnhrqvjqavykbigm1wvla-example-cabal-library-0.1.0.0.drv»
-- | ```
-- |
-- | This is assuming you're in a Nix REPL with everything from Nixpkgs
-- | available.
rawCabalFileToPkgDef :: String -> Path -> FunctionWithArgs
rawCabalFileToPkgDef rawCabalFileString packageSrc =
  case cabalParser rawCabalFileString of
    Left err -> abort $ "Could not parse cabal file: " <> show err
    Right cabalFile ->
      -- trace (show cabalFile) $
      cabalFileToPkgDef cabalFile packageSrc

-- | Take Nixpkgs as an argument and produce a `Derivation`
-- | for `rawCabalFileToPkgDef`.
-- |
-- | The derivation this produces is similar to the derivation produced from
-- | `haskellPackages.callPackage (rawCabalFileToPkgDef rawCabalFileStr haskellPackagePath) {}`.
-- |
-- | ```nix
-- | nix-repl> examplePackage pkgs
-- | «derivation /nix/store/p8ws0vwn26rhnhrqvjqavykbigm1wvla-example-cabal-library-0.1.0.0.drv»
-- | ```
-- |
-- | This is assuming you're in a Nix REPL with nixpkgs available as `pkgs`.
examplePackage :: AttrSet -> Derivation
examplePackage nixpkgs =
  (nixpkgs !. "haskellPackages" !. "callPackage")
    (rawCabalFileToPkgDef rawCabalFileStr haskellPackagePath)
    {}

-- | A normal Nixpkgs overlay that adds a top-level derivation called
-- | `exampleHaskellPackage`.
-- |
-- | This `exampleHaskellPackage` can be used like the following.
-- | Check the `../../test.nix` file for another example of how this is used:
-- |
-- | ```nix
-- | nix-repl> nixpkgs = import nixpkgs-src { overlays = [ (import ./output/Main).exampleNixpkgsOverlay ]; }
-- | nix-repl> nixpkgs.exampleHaskellPackage
-- | «derivation /nix/store/p8ws0vwn26rhnhrqvjqavykbigm1wvla-example-cabal-library-0.1.0.0.drv»
-- | ```
-- |
-- | This is assuming that `nixpkgs-src` is a `Path` to Nixpkgs`, or has been
-- | retrieved with something like `builtins.fetchTarball`.
-- |
-- | `exampleNixpkgsOverlay` is a simple example of an overlay.  It is similar
-- | a Nixpkgs overlay that looks like the following:
-- |
-- | ```nix
-- | final: _prev: {
-- |   exampleHaskellPackage = examplePackage final;
-- | }
-- | ```
exampleNixpkgsOverlay :: AttrSet -> AttrSet -> { exampleHaskellPackage :: Derivation }
exampleNixpkgsOverlay final _prev =
  { exampleHaskellPackage: examplePackage final
  }

-- | This is a Nixpkgs overlay that updates the top-level `haskell` attribute
-- | in order to add a new Haskell package, as well as the `callCabal2nixWithoutIFD`
-- | function.
-- |
-- | Here is how you could actually use this overlay.
-- |
-- | ```nix
-- | nix-repl> nixpkgs = import nixpkgs-src { overlays = [ (import ./output/Main).exampleNixpkgsHaskellOverlay ]; }
-- | nix-repl> nixpkgs.haskellPackages.exampleHaskellPackage
-- | «derivation /nix/store/p8ws0vwn26rhnhrqvjqavykbigm1wvla-example-cabal-library-0.1.0.0.drv»
-- | ```
-- |
-- | See the docs on the `exampleNixpkgsOverlay` for more information.
-- |
-- | This overlay is basically the same as the following Nix code:
-- |
-- | ```nix
-- | final: prev: {
-- |   haskell = prev.haskell // {
-- |     packageOverrides = hfinal: hprev:
-- |       prev.haskell.packageOverrides hfinal hprev // {
-- |         exampleHaskellPackage =
-- |           hfinal.callCabal2nixWithoutIFD "example-cabal-library" haskellPackagePath { };
-- |         callCabal2nixWithoutIFD = callCabal2nixWithoutIFD hfinal.callPackage
-- |       };
-- |   };
-- | }
-- | ```
exampleNixpkgsHaskellOverlay :: AttrSet -> AttrSet -> AttrSet
exampleNixpkgsHaskellOverlay _final prev =
  toAttrSet
    { haskell:
        (prev !. "haskell") //!
          { packageOverrides: \hfinal hprev ->
              (prev !. "haskell" !. "packageOverrides") hfinal hprev //!
                { exampleHaskellPackage:
                    (hfinal !. "callCabal2nixWithoutIFD")
                      "example-cabal-library"
                      haskellPackagePath
                      {}
                , callCabal2nixWithoutIFD:
                    callCabal2nixWithoutIFD (hfinal !. "callPackage")
                }
          }
    }

-- | This is just like the Nix function `callCabal2nix`, but uses all the
-- | machinery defined in this file.
-- |
-- | The first argument is the Nix function `haskellPackages.callPackage`, which
-- | is used internally.
callCabal2nixWithoutIFD
  :: (FunctionWithArgs -> AttrSet -> Derivation)
  -> String -> Path -> AttrSet -> Derivation
callCabal2nixWithoutIFD haskellCallPackage pkgName src overrides =
  let (cabalFilePath :: Path) = appendPath src ("/" <> pkgName <> ".cabal")
      (rawCabalFileString :: String) = readFile cabalFilePath
      (haskellPkgFunc :: FunctionWithArgs) = rawCabalFileToPkgDef rawCabalFileString src
  in
  haskellCallPackage haskellPkgFunc overrides

-- | This is a Row type that corresponds to the previous Haskell
-- | packages overlay (`hprev`) in the above Nix code.
-- |
-- | The only thing we need to pull out of `hprev` is `callPackage`,
-- | but this overlay contains other things, so this is an
-- | extensible row type.
type PrevHaskellPackagesRow a =
  ( callPackage :: FunctionWithArgs -> AttrSet -> Derivation
  | a
  )

-- | Turn `PrevHaskellPackagesRow` into a `Record`.
type PrevHaskellPackages s = Record (PrevHaskellPackagesRow s)

-- | This is the final Haskell packages overlay.  It corresponds
-- | to `hfinal` in the above Nix code.
-- |
-- | This should contain everything in `PrevHaskellPackagesRow`,
-- | plus the function `callCabal2nixWithoutIFDTyped`, plus
-- | `exampleHaskellPackageType`.
-- |
-- | Note that there may be more overlays on top of this, so
-- | in general when typing an overly, `PrevHaskellPackagesRow`
-- | should get a separate type variable from `FinalHaskellPackagesRow`.
type FinalHaskellPackagesRow b =
  ( callCabal2nixWithoutIFDTyped
      :: String -> Path -> AttrSet -> Derivation
  , exampleHaskellPackageTyped :: Derivation
  | PrevHaskellPackagesRow b
  )

-- | Turn `FinalHaskellPackagesRow` into a `Record`.
type FinalHaskellPackages s = Record (FinalHaskellPackagesRow s)

-- | The previous Nixpkgs overlay.
-- |
-- | Note that the only thing we pull out of the top-level is the
-- | `haskell` attribute (and `packageOverrides` inside that), so
-- | it is the only thing we have to type here.
-- |
-- | The `r` parameter represents all the additional packages that
-- | live in the Nixpkgs top-level.  We don't directly use any of
-- | them here, but `r` represents their existence.
-- |
-- | The `h` parameter represents all the additional attributes that
-- | live in the `haskell` attrset. The only thing we use from this set
-- | is `packageOverrides`, but `h` represents the existence of all
-- | the other attributes.
-- |
-- | `s` and `t` represent all the Haskell packages and other functions
-- | that live in a Haskell package set (like Nixpkgs `haskellPackages`).
-- | Note that `s` and `t` are different because the final Haskell package
-- | set may have additional packages that the previous package set doesn't
-- | have.  See the docs for `FinalHaskellPackagesRow` and `PrevHaskellPackagesRow`.
type PrevOverlayRow r h s t =
  ( haskell ::
      { packageOverrides
          :: FinalHaskellPackages s
          -> PrevHaskellPackages t
          -> FinalHaskellPackages s
      | h
      }
  | r
  )

-- | Turn `PrevOverlayRow` into a `Record`.
type PrevOverlay r h s t =
  Record (PrevOverlayRow r h s t)

-- | This is similar to `PrevOverlay` but due to how Nixpkgs
-- | overlays work, we are allowed to return an attrset with
-- | only a single attribute, and only that single attribute
-- | will be updated.
type ResultOverlay h s t =
  Record (PrevOverlayRow () h s t)

-- | This is a Nixpkgs overlay that updates the top-level `haskell` attribute
-- | in order to add a new Haskell package, as well as the
-- | `callCabal2nixWithoutIFDTyped` function.  This is the exact same as
-- | `exampleNixpkgsHaskellOverlay`, but fully typed.
-- |
-- | Here is how you could actually use this overlay.
-- |
-- | ```nix
-- | nix-repl> nixpkgs = import nixpkgs-src { overlays = [ (import ./output/Main).exampleNixpkgsHaskellOverlayTyped ]; }
-- | nix-repl> nixpkgs.haskellPackages.exampleHaskellPackageTyped
-- | «derivation /nix/store/p8ws0vwn26rhnhrqvjqavykbigm1wvla-example-cabal-library-0.1.0.0.drv»
-- | ```
-- |
-- | See the docs on the `exampleNixpkgsOverlay` for more information.
-- |
-- | This overlay is basically the same as the following Nix code:
-- |
-- | ```nix
-- | final: prev: {
-- |   haskell = prev.haskell // {
-- |     packageOverrides = hfinal: hprev:
-- |       prev.haskell.packageOverrides hfinal hprev // {
-- |         exampleHaskellPackageTyped =
-- |           hfinal.callCabal2nixWithoutIFD
-- |             "example-cabal-library"
-- |             haskellPackagePath
-- |             { };
-- |         callCabal2nixWithoutIFDTyped = callCabal2nixWithoutIFD hfinal.callPackage
-- |       };
-- |   };
-- | }
-- | ```
-- |
-- | Adding types to this function is quite annoying and tricky to figure out,
-- | but the resulting PureScript code looks almost identical to the untyped
-- | Nix code.
exampleNixpkgsHaskellOverlayTyped
  :: forall p r h s t
   . Record p
  -> PrevOverlay r h s t
  -> ResultOverlay h s t
exampleNixpkgsHaskellOverlayTyped _final prev =
  { haskell:
      prev.haskell
        { packageOverrides = \hfinal hprev ->
            (prev.haskell.packageOverrides hfinal hprev)
              { exampleHaskellPackageTyped =
                  hfinal.callCabal2nixWithoutIFDTyped
                    "example-cabal-library"
                    haskellPackagePath
                    (toAttrSet {})
              , callCabal2nixWithoutIFDTyped =
                  callCabal2nixWithoutIFD hfinal.callPackage
              }
        }
  }
