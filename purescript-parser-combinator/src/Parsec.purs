module Parsec
  -- ( Parser,
  --   runParser,
  --   token,
  --   throwAt,
  -- )
where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative, (<|>))
import Control.MonadPlus (class MonadPlus)
import Control.Lazy (class Lazy)
import Control.Plus (class Plus, empty)
import Data.Array (foldr, replicate, some, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), optional)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\), type (/\))
import NixBuiltins (charToStr, concatChars, stringLength, substring, trace, unsafeStrToChar)

data Err' e = Err Int e

instance semigroupErr :: Semigroup e => Semigroup (Err' e) where
  append (Err il el) (Err ir er) = case compare il ir of
    LT -> Err ir er
    EQ -> Err ir (append el er)
    GT -> Err il el

instance monoidErr :: Monoid e => Monoid (Err' e) where
  mempty = Err 0 mempty

type Err = Err' (Array String)

-- | A simple backtracking parser.
-- Maintains the error message of whatever branch managed to consume the most input.
newtype Parser a = Parser
  ( forall b.
    String ->
    Int -> -- Offset
    Err -> -- error set
    (a -> Int -> Err -> b) -> -- Success continuation
    (Err -> b) -> -- Error continuation
    b
  )

unParser
  :: forall b a
   . Parser a -> 
     String ->
     Int -> -- Offset
     Err -> -- error set
     (a -> Int -> Err -> b) -> -- Success continuation
     (Err -> b) -> -- Error continuation
     b
unParser (Parser p) = p

runParser ::
  forall a.
  String ->
  Parser a ->
  Either (Int /\ Array String) (Int /\ a)
runParser s (Parser p) =
  p
    s
    0
    mempty
    (\a i _ -> Right (i /\ a))
    (\(Err i e) -> Left (i /\ e))

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \s i e ok err ->
    p s i e (ok <<< f) err

instance applyParser :: Apply Parser where
  apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  apply (Parser pf) (Parser pa) = Parser \t i e ok ng ->
    pf t i e (\f i' e' -> pa t i' e' (ok <<< f) ng) ng

instance applicativeParser :: Applicative Parser where
  pure a = Parser \_ i e ok _ -> ok a i e

instance bindParser :: Bind Parser where
  bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
  bind (Parser k) f = Parser \t i e ok ng ->
    k t i e (\a i' e' -> unParser (f a) t i' e' ok ng) ng

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt (Parser pl) (Parser pr) = Parser \t i e ok ng ->
    pl t i e ok \e' -> pr t i e' ok ng

instance plusParser :: Plus Parser where
  empty = Parser \_ _ e _ ng -> ng e

instance alternativeParser :: Alternative Parser

instance monadPlusParser :: MonadPlus Parser

-- This isn't actually needed, since PureNix is already Lazy, but some things in
-- the PureScript stdlib are using Lazy.
instance lazyParser :: Lazy (Parser a) where
  defer :: (Unit -> Parser a) -> Parser a
  defer f = Parser \t i e ok ng -> unParser (f unit) t i e ok ng

----------------
-- Primitives --
----------------

chars :: Int -> Parser String
chars n = Parser \str i err ok ng ->
  let endOffset = i + n
  in
  if stringLength str >= endOffset then
    ok (substring i n str) endOffset err
  else
    let errMsg = "not enough characters left in Parser to parse " <> show n <> " chars"
    in
    ng (err <> Err i [errMsg])

-- optional :: forall a. Parser a -> Parser (Maybe a)
-- optional p = Parser go
--   where
--   go
--     :: forall b
--      . String
--     -> Int
--     -> Err
--     -> (Maybe a -> Int -> Err -> b)
--     -> (Err -> b)
--     -> b
--   go str i err ok _ =
--     let newOk :: a -> Int -> Err -> b
--         newOk s i' err' = ok (Just s) i' err'

--         newNg :: Err -> b
--         newNg _ = ok Nothing i err
--     in
--     unParser p str i err newOk newNg

eof :: Parser Unit
eof = Parser \str i err ok ng ->
  if i >= stringLength str then
    ok unit i err
  else
    ng (err <> Err i ["not at eof"])

-- test :: Parser String
-- test = throwAt \throw -> do
--   res1 <- optional (chars 3)
--   res2 <- optional (chars 3)
--   case res1, res2 of
--     Nothing, _ -> throw "no first value yo"
--     Just _, Nothing -> throw "no second value yo"
--     Just str1, Just str2 -> pure $ str1 <> str2

-- | Enter a context with a function to throw an error at the start of the context.
-- | A simple motivating example is `expect`:
-- |
-- | ```purescript
-- | expect :: forall a. Error -> (String -> Maybe a) -> Parser a
-- | expect err f =
-- |   P.throwAt \throw ->
-- |      P.token >>= (maybe (throw err) pure <<< f)
-- | ```
-- |
-- | In this example, we first consume a token, and then see if it matches our
-- | expectation. However since consuming a token advanced the parser past the
-- | token, simply throwing an error in place reports the error after the token.
-- |
-- | By having `throwAt` be the only way to throw errors, it's always clear and
-- | explicit where you are reporting an error.
throwAt
  :: forall r
   . ((forall a. String -> Parser a) -> Parser r)
  -> Parser r
throwAt k = Parser \str i err ok ng ->
  let throw' :: forall x. String -> Parser x
      throw' e = Parser \_ _ e' _ ng' -> ng' (e' <> Err i [e])
  in unParser (k throw') str i err ok ng

-----------------
-- Combinators --
-----------------

char :: Char -> Parser Unit
char c =
  void $
    satisfyNote
      (_ == c)
      (\parsedChar ->
        "expected character '" <> charToStr c <>
        "', but got '" <> charToStr parsedChar <> "'"
      )

notChar :: Char -> Parser Char
notChar c =
  satisfyNote
    (_ /= c)
    (\parsedChar ->
      "expected any character but '" <> charToStr c <>
      "', but got '" <> charToStr parsedChar
    )

satisfyNote :: (Char -> Boolean) -> (Char -> String) -> Parser Char
satisfyNote f errMsg =
  throwAt \throw -> do
    parsedChar <- map unsafeStrToChar (chars 1)
    if f parsedChar
      then pure parsedChar
      else throw (errMsg parsedChar)

isAlphaNum :: Char -> Boolean
isAlphaNum =
  case _ of
    'A' -> true
    'B' -> true
    'C' -> true
    'D' -> true
    'E' -> true
    'F' -> true
    'G' -> true
    'H' -> true
    'I' -> true
    'J' -> true
    'K' -> true
    'L' -> true
    'M' -> true
    'N' -> true
    'O' -> true
    'P' -> true
    'Q' -> true
    'R' -> true
    'S' -> true
    'T' -> true
    'U' -> true
    'V' -> true
    'W' -> true
    'X' -> true
    'Y' -> true
    'Z' -> true
    'a' -> true
    'b' -> true
    'c' -> true
    'd' -> true
    'e' -> true
    'f' -> true
    'g' -> true
    'h' -> true
    'i' -> true
    'j' -> true
    'k' -> true
    'l' -> true
    'm' -> true
    'n' -> true
    'o' -> true
    'p' -> true
    'q' -> true
    'r' -> true
    's' -> true
    't' -> true
    'u' -> true
    'v' -> true
    'w' -> true
    'x' -> true
    'y' -> true
    'z' -> true
    '0' -> true
    '1' -> true
    '2' -> true
    '3' -> true
    '4' -> true
    '5' -> true
    '6' -> true
    '7' -> true
    '8' -> true
    '9' -> true
    _ -> false

alphaNum :: Parser Char
alphaNum =
  satisfyNote
    isAlphaNum
    (\c ->
      "Trying to parse A-Z or a-z or 0-9, but got character '" <> charToStr c <> "'"
    )

alphaNums :: Parser String
alphaNums = map concatChars (some alphaNum)

space :: Parser Unit
space = char ' ' <|> char '\t'

count :: forall a. Int -> Parser a -> Parser (Array a)
count i p = sequence $ replicate i p

string :: String -> Parser String
string s =
  throwAt \throw -> do
    parsedChars <- chars (stringLength s)
    if s == parsedChars
      then pure s
      else throw $ "expected string '" <> s <> "', but got string '" <> parsedChars <> "'"

oneOf :: forall a. Array (Parser a) -> Parser a
oneOf parsers = foldr alt empty parsers
