module Libs.Expr where

data LispExpr = LispInt Integer
              | LispSymbol String

instance Show LispExpr where
  show :: LispExpr -> String
  show (LispInt i)    = show i
  show (LispSymbol s) = s
