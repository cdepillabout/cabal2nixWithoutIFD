{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Nix.Prelude

import Data.Aeson (decode)
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Language.PureScript.CoreFn (modulePath)
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Nix.Convert (convert)
import Nix.Print (renderExpr)
import Nix.Util (stripAnnMod)
import System.Directory (createDirectoryIfMissing)
import Text.Pretty.Simple (pPrint)
import System.Directory (copyFile, doesFileExist)
import qualified System.Environment as Env
import qualified System.Exit as Sys
import System.FilePath (replaceExtension)

defaultMain :: IO ()
defaultMain = do
  Env.getArgs >>= print
  let modName = "Main"
      moduleDir = "../purescript-cabal-parser/output/" <> modName <> "/"
      corefnFilePath = moduleDir <> "corefn.json"
  corefn <- readFile corefnFilePath
  -- TODO why is this lazy?
  let maybeValue = decode $ encodeUtf8 $ pack corefn
  case maybeValue of
    Nothing -> error "wasn't able to decode corefn as json"
    Just value -> do
      let eitherModule = parseEither moduleFromJSON value
      case eitherModule of
        Left err -> error err
        Right (_version, mdl) -> do
          putStrLn "successfully decoded purescript module:"
          pPrint (stripAnnMod mdl)

          -- Create the output __ffi.nix file for this module if it has foreign imports.
          let ffiFilePath = "../purescript-cabal-parser/" <> replaceExtension (modulePath mdl) "nix"
          let ffiOutputFilePath = moduleDir <> "__ffi.nix"
          doesFfiFileExist <- doesFileExist ffiFilePath
          when doesFfiFileExist $ copyFile ffiFilePath ffiOutputFilePath

          let eitherNixExpr = convert mdl
          -- pPrint eitherNixExpr
          nix <- either (Sys.die . T.unpack) (pure . renderExpr) eitherNixExpr
          TL.putStrLn nix

          -- TODO: Transform the purescript module into a .nix file
          createDirectoryIfMissing True "../generated"
          putStrLn "writing ../generated/Main.nix"
          TL.writeFile "../generated/Main.nix" nix
