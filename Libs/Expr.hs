module Libs.Expr where

import qualified Data.Map as Map

data LispExpr = LispInt Integer
              | LispSymbol String

type Context = Map.Map String LispExpr

instance Show LispExpr where
  show :: LispExpr -> String
  show (LispInt i)    = show i
  show (LispSymbol s) = s

eval :: Context -> LispExpr -> LispExpr
eval ctx (LispInt i)    = (LispInt i)
eval ctx (LispSymbol s) = ctx Map.! s
