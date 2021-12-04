
module NixBuiltins where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Path :: Type

foreign import concatStringsSep :: String -> Array String -> String

foreign import readFile :: Path -> String

foreign import stringLength :: String -> Int

foreign import substring :: Int -> Int -> String -> String

foreign import trace :: forall a. String -> a -> a

charToStr :: Char -> String
charToStr = unsafeCoerce

charArrayToStrArray :: Array Char -> Array String
charArrayToStrArray = unsafeCoerce

concatChars :: Array Char -> String
concatChars = unsafeCoerce concatStrs

concatStrs :: Array String -> String
concatStrs = concatStringsSep ""

unsafeStrToChar :: String -> Char
unsafeStrToChar = unsafeCoerce
