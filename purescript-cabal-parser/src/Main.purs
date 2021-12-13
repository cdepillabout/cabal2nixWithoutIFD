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

undefined :: forall a. a
undefined = unsafeCoerce unit

type Executable =
  { name :: String
  , buildDepends :: Array String
  }

data License = LicenseBSD3

derive instance genericLicense :: Generic License _

instance showLicense :: Show License where
  show = genericShow

type CabalFile =
  { name :: String
  , version :: String
  , license :: License
  , executable :: Executable
  }

type IndentAmount = Int

data RawProp = SimpleRawProp String String | RecursiveRawProp String String (Array RawProp)

derive instance genericRawProp :: Generic RawProp _

instance showRawProp :: Show RawProp where
  show rawProp = genericShow rawProp

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

cabalParser :: String -> Either (Array String) CabalFile
cabalParser rawCabalFileString = do
  case parsedRawCabalFile rawCabalFileString of
    Left (_ /\ err) -> Left err
    Right (_ /\ rawCabalFile) -> cabalFileFromRaw rawCabalFile

licenseToAttrPath :: License -> Array String
licenseToAttrPath =
  case _ of
    LicenseBSD3 -> ["lib", "licenses", "bsd3"]

foreign import data FunctionWithArgs :: Type

foreign import haskellPackagePath :: Path

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
-- | This is similar to doing
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

callCabal2nixWithoutIFD
  :: (FunctionWithArgs -> AttrSet -> Derivation)
  -> String -> Path -> AttrSet -> Derivation
callCabal2nixWithoutIFD haskellCallPackage pkgName src overrides =
  let (cabalFilePath :: Path) = appendPath src ("/" <> pkgName <> ".cabal")
      (rawCabalFileString :: String) = readFile cabalFilePath
      (haskellPkgFunc :: FunctionWithArgs) = rawCabalFileToPkgDef rawCabalFileString src
  in
  haskellCallPackage haskellPkgFunc overrides
