{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Nix.Expr where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Nix.Prelude

type Ident = Text

newtype Expr = Expr {unExpr :: ExprF Expr}
  deriving newtype (Show)

data ExprF f
  = Var Ident
  | Lam Ident f
  | App f f
  | Attrs [Ident] [(f, [Ident])] [(Ident, f)]
  | Cond f f f
  | List [f]
  | Bin Op f f
  | Not f
  | Sel f Ident
  | Let (NonEmpty (Ident, f)) f
  | Int Integer
  | Double Double
  | String Text
  | Path Text
  deriving stock (Functor, Foldable, Traversable, Show)

data Op = Update | Equals | And
  deriving (Eq, Show)

foldExpr :: (ExprF r -> r) -> Expr -> r
foldExpr f = go where go = f . fmap go . unExpr

var :: Ident -> Expr
var = Expr . Var

lam :: Ident -> Expr -> Expr
lam arg body = Expr $ Lam arg body

app :: Expr -> Expr -> Expr
app f x = Expr $ App f x

cond :: Expr -> Expr -> Expr -> Expr
cond c true false = Expr $ Cond c true false

attrs ::
  [Ident] ->
  [(Expr, [Ident])] ->
  [(Ident, Expr)] ->
  Expr
attrs inherits inheritFroms binds = Expr $ Attrs inherits inheritFroms binds

sel :: Expr -> Ident -> Expr
sel e s = Expr $ Sel e s

let' :: [(Ident, Expr)] -> Expr -> Expr
let' [] body = body
let' (h : t) body = Expr $ Let (h :| t) body

int :: Integer -> Expr
int = Expr . Int

double :: Double -> Expr
double = Expr . Double

string :: Text -> Expr
string = Expr . String

list :: [Expr] -> Expr
list = Expr . List

bin :: Op -> Expr -> Expr -> Expr
bin op a b = Expr $ Bin op a b

path :: Text -> Expr
path = Expr . Path

numberedNames :: Text -> [Ident]
numberedNames prefix = fmap (\n -> prefix <> T.pack (show n)) [0 :: Int ..]

constructorFieldNames :: [Ident]
constructorFieldNames = numberedNames "__field"

negate :: Expr -> Expr
negate = Expr . Not

builtin :: Text -> Expr
builtin = sel (var "builtins")

--   Just
-- becomes
--   (a: { __tag = "Just"; __field0 = a; })
constructor :: Ident -> [Ident] -> Expr
constructor conName fields =
  foldr
    lam
    ( attrs
        []
        []
        (("__tag", string conName) : zipWith (\arg name -> (name, var arg)) fields constructorFieldNames)
    )
    fields
