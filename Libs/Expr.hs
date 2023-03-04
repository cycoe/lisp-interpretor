
module Libs.Expr where

import qualified Data.Map as Map

data LispExpr = LispInt Integer
              | LispSymbol String
              | LispFunc ([LispExpr] -> LispExpr)
              | LispList [LispExpr]

type Context = Map.Map String LispExpr

instance Show LispExpr where
  show :: LispExpr -> String
  show (LispInt i)    = show i
  show (LispSymbol s) = s
  show (LispFunc _) = "<function>"
  show (LispList xs) = "(" ++ unwords (show <$> xs) ++")"

eval :: Context -> LispExpr -> LispExpr
eval ctx (LispInt i)        = LispInt i
eval ctx (LispSymbol s)     = ctx Map.! s
eval ctx (LispFunc f)       = LispFunc f
eval ctx (LispList (x:xs))  = apply (eval ctx x) (eval ctx <$> xs) where
  apply :: LispExpr -> [LispExpr] -> LispExpr
  apply (LispFunc f) args = f args
  apply _ args = undefined

intBinaryOp :: (Integer -> Integer -> Integer) -> [LispExpr] -> LispExpr
intBinaryOp op (x:xs) =LispInt $ foldl op (unwrapInt x) (map unwrapInt xs) where
  unwrapInt :: LispExpr -> Integer
  unwrapInt (LispInt i) = i
  unwrapInt _           = undefined

symbols :: Context
symbols = Map.fromList
  [ ("+", LispFunc (intBinaryOp (+)))
  , ("-", LispFunc (intBinaryOp (-)))
  , ("*", LispFunc (intBinaryOp (*)))
  , ("/", LispFunc (intBinaryOp div))
  ]
