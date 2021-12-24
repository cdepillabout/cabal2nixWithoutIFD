
module NixBuiltins where

import Unsafe.Coerce (unsafeCoerce)

-- | A Nix attribute set that can have any types as values.  This is generally
-- | used from unsafe functions like `getAttr`.
foreign import data AttrSet :: Type

foreign import data Derivation :: Type

foreign import data Path :: Type

foreign import abort :: forall a. String  -> a

foreign import attrByPath :: forall a. Array String  -> a -> AttrSet -> a

foreign import attrUpdate :: AttrSet -> AttrSet -> AttrSet
infixr 5 attrUpdate as //

foreign import attrUpdate' :: forall r. AttrSet -> { | r } -> AttrSet
infixr 5 attrUpdate' as //!

foreign import concatStringsSep :: String -> Array String -> String

foreign import getAttr :: forall a. String -> AttrSet -> a

foreign import getAttrFromPath :: forall a. Array String  -> AttrSet -> a

foreign import readFile :: Path -> String

foreign import stringLength :: String -> Int

foreign import substring :: Int -> Int -> String -> String

foreign import trace :: forall a. String -> a -> a

foreign import unsafeAdd :: forall a b c. a -> b -> c

appendPath :: Path -> String -> Path
appendPath = unsafeAdd

charToStr :: Char -> String
charToStr = unsafeCoerce

charArrayToStrArray :: Array Char -> Array String
charArrayToStrArray = unsafeCoerce

concatChars :: Array Char -> String
concatChars = unsafeCoerce concatStrs

concatStrs :: Array String -> String
concatStrs = concatStringsSep ""

-- | A flipped version of `getAttr`.
getAttrFlip :: forall a. AttrSet -> String -> a
getAttrFlip attrSet key = getAttr key attrSet
infixl 9 getAttrFlip as !.

toAttrSet :: forall r. {|r} -> AttrSet
toAttrSet = unsafeCoerce

unsafeStrToChar :: String -> Char
unsafeStrToChar = unsafeCoerce
