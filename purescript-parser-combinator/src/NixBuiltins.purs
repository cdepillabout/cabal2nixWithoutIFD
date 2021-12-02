
module NixBuiltins where

import Unsafe.Coerce (unsafeCoerce)

foreign import stringLength :: String -> Int

foreign import substring :: Int -> Int -> String -> String

foreign import concatLists :: Array String -> String

charToStr :: Char -> String
charToStr = unsafeCoerce

unsafeStrToChar :: String -> Char
unsafeStrToChar = unsafeCoerce

charArrayToStr :: Array Char -> String
charArrayToStr = unsafeCoerce concatLists
