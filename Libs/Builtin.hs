module Libs.Builtin where

import qualified Data.Map as Map
import Libs.Expr (LispExpr(..), Context)

lispSet :: Context -> [LispExpr] -> (Context, LispExpr)
lispSet ctx [LispSymbol s, expr] = (Map.insert s expr ctx, expr)

intBinaryOp :: (Integer -> Integer -> Integer) -> Context -> [LispExpr] -> (Context, LispExpr)
intBinaryOp op ctx (x:xs) = (ctx, LispInt $ foldl op (unwrapInt x) (map unwrapInt xs)) where
  unwrapInt :: LispExpr -> Integer
  unwrapInt (LispInt i) = i
  unwrapInt _           = undefined

symbols :: Context
symbols = Map.fromList
  [ ("set", LispQuot lispSet)
  , ("+", LispFunc (intBinaryOp (+)))
  , ("-", LispFunc (intBinaryOp (-)))
  , ("*", LispFunc (intBinaryOp (*)))
  , ("/", LispFunc (intBinaryOp div))
  ]
