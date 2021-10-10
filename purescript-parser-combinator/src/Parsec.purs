module Parsec
  -- ( Parser,
  --   runParser,
  --   token,
  --   throwAt,
  -- )
where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import NixBuiltins (stringLength, substring)

data Err' e = Err Int e

instance semigroupErr :: Semigroup e => Semigroup (Err' e) where
  append (Err il el) (Err ir er) = case compare il ir of
    LT -> Err ir er
    EQ -> Err ir (append el er)
    GT -> Err il el

instance monoidErr :: Monoid e => Monoid (Err' e) where
  mempty = Err 0 mempty

type Err = Err' String

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
  Either (Int /\ String) (Int /\ a)
runParser s (Parser p) =
  p
    s
    0
    mempty
    (\a i _ -> Right (i /\ a))
    (\(Err i e) -> Left (i /\ e))

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \s i e ok err -> p s i e (ok <<< f) err

instance applyParser :: Apply Parser where
  apply (Parser pf) (Parser pa) = Parser \t i e ok ng -> pf t i e (\f i' e' -> pa t i' e' (ok <<< f) ng) ng

instance applicativeParser :: Applicative Parser where
  pure a = Parser \_ i e ok _ -> ok a i e

instance bindParser :: Bind Parser where
  bind (Parser k) f = Parser \t i e ok ng -> k t i e (\a i' e' -> unParser (f a) t i' e' ok ng) ng

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt (Parser pl) (Parser pr) = Parser \t i e ok ng ->
    pl t i e ok $ \e' ->
      pr t i e' ok ng

instance plusParser :: Plus Parser where
  empty = Parser \_ _ e _ ng -> ng e

instance alternativeParser :: Alternative Parser

chars :: Int -> Parser String
chars n = Parser \str i err ok ng ->
  let endOffset = i + n
  in
  if stringLength str >= endOffset then
    ok (substring i endOffset str) endOffset err
  else
    let errMsg = "not enough characters left in Parser to parse " <> show n <> " chars"
    in
    ng (err <> Err i errMsg)

eof :: Parser Unit
eof = Parser \str i err ok ng ->
  if i >= stringLength str then
    ok unit i err
  else
    ng (err <> Err i "not at eof")

-- {-# INLINE token #-}
-- token :: Parser t e t
-- token = Parser $ \t i e ok _ -> ok (t i) (i + 1) e

-- {-# INLINE throwAt #-}
-- throwAt :: Semigroup e => ((forall err. e -> Parser t e err) -> Parser t e a) -> Parser t e a
-- throwAt k = Parser $ \t i e ok err ->
--   let throw' e = Parser $ \_ _ e' _ err' -> err' (e' <> Err i e)
--    in unParser (k throw') t i e ok err
