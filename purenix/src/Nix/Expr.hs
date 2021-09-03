{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nix.Expr where

import Data.Map (Map)
import Data.Text (Text)

type Ident = Text

newtype Expr = Expr {unExpr :: ExprF Expr}
  deriving newtype (Show)

data ExprF f
  = Var Ident
  | Abs Ident f
  | App f f
  | Attrs [Ident] [(f, [Ident])] [(Ident, f)]
  | List [f]
  | Bin Op f f
  | Sel f Ident
  | Let [(Ident, f)] f
  | Num Integer
  | String Text
  deriving stock (Functor, Foldable, Traversable, Show)

data Op = Update
  deriving (Eq, Show)

foldExpr :: (ExprF r -> r) -> Expr -> r
foldExpr f = go where go = f . fmap go . unExpr

var :: Ident -> Expr
var = Expr . Var

abs :: Ident -> Expr -> Expr
abs arg body = Expr $ Abs arg body

app :: Expr -> Expr -> Expr
app f x = Expr $ App f x

attrs ::
  [Ident] ->
  [(Expr, [Ident])] ->
  [(Ident, Expr)] ->
  Expr
attrs inherits inheritFroms binds = Expr $ Attrs inherits inheritFroms binds

sel :: Expr -> Ident -> Expr
sel e s = Expr $ Sel e s

let' :: [(Ident, Expr)] -> Expr -> Expr
let' binds body = Expr $ Let binds body

num :: Integer -> Expr
num = Expr . Num

string :: Text -> Expr
string = Expr . String

list :: [Expr] -> Expr
list = Expr . List

bin :: Op -> Expr -> Expr -> Expr
bin op a b = Expr $ Bin op a b
