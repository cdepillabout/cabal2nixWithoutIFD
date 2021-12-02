module Main where

import Prelude
import Parsec

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

parseRawCabalFile :: Parser RawCabalFile

-- cabalParser :: Parser CabalFile
-- cabalParser = do
--   rawCabalFile <- parseRawCabalFile
--   pure undefined

