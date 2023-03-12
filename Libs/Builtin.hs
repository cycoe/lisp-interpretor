module Libs.Builtin where

import qualified Data.Map as Map
import Libs.Expr (LispExpr(..), Context, LispState, FunctionSignature, eval, getSymbols, getSymbol)
import Control.Monad.State (modify, MonadState (get))

lispSetArgs :: FunctionSignature
lispSetArgs = ["symbol", "expr"]
lispSet :: LispState
lispSet = do
  [LispSymbol s, expr] <- getSymbols lispSetArgs
  eval_e <- eval expr
  modify $ Map.insert s eval_e
  return eval_e

lispLambdaArgs :: FunctionSignature
lispLambdaArgs = ["args", "body"]
lispLambda :: LispState
lispLambda = do
  [LispList args, body] <- getSymbols lispLambdaArgs
  return $ LispFunc (eval body) ((\(LispSymbol arg) -> arg) <$> args)

intBinaryOp :: (Integer -> Integer -> Integer) -> LispState
intBinaryOp op = do
  LispList (x:xs) <- getSymbol "..."
  return . LispInt $ foldl op (unwrapInt x) (map unwrapInt xs) where
  unwrapInt :: LispExpr -> Integer
  unwrapInt (LispInt i) = i
  unwrapInt expr        = undefined

symbols :: Context
symbols = Map.fromList
  [ ("set", LispQuot lispSet lispSetArgs)
  , ("lambda", LispQuot lispLambda lispLambdaArgs)
  , ("+", LispFunc (intBinaryOp (+)) ["..."])
  , ("-", LispFunc (intBinaryOp (-)) ["..."])
  , ("*", LispFunc (intBinaryOp (*)) ["..."])
  , ("/", LispFunc (intBinaryOp div) ["..."])
  ]
