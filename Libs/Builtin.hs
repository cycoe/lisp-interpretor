module Libs.Builtin where

import qualified Data.Map as Map
import Libs.Expr (LispExpr(..), Context, LispState)
import Control.Monad.State (modify)

lispSet :: [LispExpr] -> LispState
lispSet [LispSymbol s, expr] = do
  modify $ Map.insert s expr
  return expr

intBinaryOp :: (Integer -> Integer -> Integer) -> [LispExpr] -> LispState
intBinaryOp op (x:xs) = return . LispInt $ foldl op (unwrapInt x) (map unwrapInt xs) where
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
