module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (many, some)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import NixBuiltins (Path, concatChars, readFile, trace)
import Parsec
import Unsafe.Coerce (unsafeCoerce)

undefined :: forall a. a
undefined = unsafeCoerce unit

type Executable =
  { name :: String
  , buildDepends :: Array String
  }

data License = LicenseBSD3

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
parseRawCabalFile = map RawCabalFile (parseRawProps 0)

foreign import cabalFilePath :: Path

rawCabalFileStr :: String
rawCabalFileStr = readFile cabalFilePath

parsedRawCabalFile ::  Either (Int /\ Array String) (Int /\ RawCabalFile)
parsedRawCabalFile = runParser rawCabalFileStr parseRawCabalFile

cabalParser :: Either (Array String) CabalFile
cabalParser = do
  case parsedRawCabalFile of
    Left (_ /\ err) -> Left err
    Right (_ /\ rawCabalFile) -> trace (show rawCabalFile) undefined -- (Right rawCabalFile)
