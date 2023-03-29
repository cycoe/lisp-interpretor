module Libs.Builtin where

import qualified Data.Map as Map
import Libs.Expr (LispExpr(..), Context(..), LispState, FunctionSignature, eval, getSymbols, getSymbol, updateSymbolInParent)
import Control.Monad.State (modify, MonadState (get))
import Control.Monad.Except (MonadError(throwError))

lispSetArgs :: FunctionSignature
lispSetArgs = ["symbol", "expr"]
lispSet :: LispState
lispSet = do
  [LispSymbol s, expr] <- getSymbols lispSetArgs
  eval_e <- eval expr
  updateSymbolInParent s eval_e
  return eval_e

lispProg :: LispState
lispProg = do
  LispList exprs <- getSymbol "..."
  results <- mapM eval exprs
  case results of
    [] -> throwError "[lispProg] prog list CANNOT be empty!"
    rs -> return $ last rs

lispLambdaArgs :: FunctionSignature
lispLambdaArgs = ["args", "body"]
lispLambda :: LispState
lispLambda = do
  [LispList args, body] <- getSymbols lispLambdaArgs
  return $ LispFunc (eval body) ((\(LispSymbol arg) -> arg) <$> args)

lispIfArgs :: FunctionSignature
lispIfArgs = ["cond", "expr1", "expr2"]
lispIf :: LispState
lispIf = do
  [cond, expr1, expr2] <- getSymbols lispIfArgs
  econd <- eval cond
  case econd of
    LispInt i -> eval $ if i /= 0 then expr1 else expr2
    e         -> throwError $ "[lispIf] expr [" ++ show e
                              ++ "] CANNOT be a condition!"

lispCmpArgs :: FunctionSignature
lispCmpArgs = ["left", "right"]
lispCmp :: (Integer -> Integer -> Bool) -> LispState
lispCmp op = do
  [l, r] <- getSymbols lispCmpArgs
  case (l, r) of
    (LispInt li, LispInt ri) -> return . LispInt $ if li `op` ri then 1 else 0
    (lo, ro) -> throwError $ "[lispCmp] expr1 [" ++ show lo
                          ++ "] and expr2[" ++ show ro ++ "] MUST be LispInt!"

lispNotArgs :: FunctionSignature
lispNotArgs = ["cond"]
lispNot :: LispState
lispNot = do
  [cond] <- getSymbols lispNotArgs
  case cond of
    LispInt i -> return . LispInt $ if i == 0 then 1 else 0
    other -> throwError $ "[lispNot] cond [" ++ show other ++ "] MUST be LispInt!"

lispQuote :: LispState
lispQuote = getSymbol "..."

lispCarArgs :: FunctionSignature
lispCarArgs = ["list"]
lispCar :: LispState
lispCar = do
  [expr] <- getSymbols lispCarArgs
  case expr of
    LispList (x:_) -> return x
    LispList []    -> throwError "[lispCar] Empty list!"
    unhandled      -> throwError $ "[lispCar] CANNOT apply car on [" ++ show unhandled ++ "]!"

lispCdrArgs :: FunctionSignature
lispCdrArgs = ["list"]
lispCdr :: LispState
lispCdr = do
  [expr] <- getSymbols lispCdrArgs
  case expr of
    LispList (_:xs) -> return $ LispList xs
    LispList []     -> throwError "[lispCdr] Empty list!"
    unhandled       -> throwError $ "[lispCdr] CANNOT apply cdr on [" ++ show unhandled ++ "]!"

lispConsArgs :: FunctionSignature
lispConsArgs = ["item", "list"]
lispCons :: LispState
lispCons = do
  [x, expr] <- getSymbols lispConsArgs
  case expr of
    LispList xs -> return . LispList $ x:xs
    unhandled   -> throwError $ "[lispCons] CANNOT cons with [" ++ show unhandled ++ "]!"

lispMapArgs :: FunctionSignature
lispMapArgs = ["func", "list"]
lispMap :: LispState
lispMap = do
  [f, l] <- getSymbols lispMapArgs
  case l of
    LispList xs -> LispList <$> mapM (eval . LispList . (f:) . (:[]))  xs
    unhandled   -> throwError $ "[lispMap] Cannot apply map on [" ++ show unhandled ++ "]!"

intBinaryOp :: (Integer -> Integer -> Integer) -> LispState
intBinaryOp op = do
  LispList (x:xs) <- getSymbol "..."
  return . LispInt $ foldl op (unwrapInt x) (map unwrapInt xs) where
  unwrapInt :: LispExpr -> Integer
  unwrapInt (LispInt i) = i
  unwrapInt expr        = undefined

symbols :: Context
symbols = Context (Map.fromList
  [ ("set", LispQuot lispSet lispSetArgs)
  , ("prog", LispQuot lispProg ["..."])
  , ("lambda", LispQuot lispLambda lispLambdaArgs)
  , ("if", LispQuot lispIf lispIfArgs)
  , ("eq", LispFunc (lispCmp (==)) lispCmpArgs)
  , ("ne", LispFunc (lispCmp (/=)) lispCmpArgs)
  , ("gt", LispFunc (lispCmp (>)) lispCmpArgs)
  , ("ge", LispFunc (lispCmp (>=)) lispCmpArgs)
  , ("lt", LispFunc (lispCmp (<)) lispCmpArgs)
  , ("le", LispFunc (lispCmp (<=)) lispCmpArgs)
  , ("not", LispFunc lispNot lispNotArgs)
  , ("quot", LispQuot lispQuote ["..."])
  , ("car", LispFunc lispCar lispCarArgs)
  , ("cdr", LispFunc lispCdr lispCdrArgs)
  , ("cons", LispFunc lispCons lispConsArgs)
  , ("map", LispFunc lispMap lispMapArgs)
  , ("+", LispFunc (intBinaryOp (+)) ["..."])
  , ("-", LispFunc (intBinaryOp (-)) ["..."])
  , ("*", LispFunc (intBinaryOp (*)) ["..."])
  , ("/", LispFunc (intBinaryOp div) ["..."])
  ]) Nothing
