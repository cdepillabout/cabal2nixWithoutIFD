
module NixBuiltins where

foreign import stringLength :: String -> Int

foreign import substring :: Int -> Int -> String -> String
