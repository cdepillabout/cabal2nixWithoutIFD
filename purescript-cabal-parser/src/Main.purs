module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (many)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import NixBuiltins (concatChars)
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

data RawProp = SimpleRawProp String String | RecursiveRawProp String RawProp

data RawCabalFile = RawCabalFile (Array RawProp)

parseRawPropKey :: Parser String
parseRawPropKey = map concatChars (many (notChar ':'))

parseRawPropVal :: Parser String
parseRawPropVal = do
  val <- map concatChars (many (notChar '\n'))
  char '\n'
  pure val

parseSimpleRawProp :: Parser RawProp
parseSimpleRawProp = do
  key <- parseRawPropKey
  char ':'
  val <- parseRawPropVal
  pure $ SimpleRawProp key val

parseRecursiveRawProp :: Parser RawProp
parseRecursiveRawProp = empty

parseRawProp :: Parser RawProp
parseRawProp = parseRecursiveRawProp <|> parseSimpleRawProp

parseRawCabalFile :: Parser RawCabalFile
parseRawCabalFile = map RawCabalFile (many parseRawProp)

-- cabalParser :: Parser CabalFile
-- cabalParser = do
--   rawCabalFile <- parseRawCabalFile
--   pure undefined

