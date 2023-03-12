module Libs.Expr where

import qualified Data.Map as Map
import Data.List (mapAccumL)
import Control.Monad.State (StateT, MonadState (get), modify)
import Control.Monad.Except (ExceptT, MonadError (throwError))

data LispExpr = LispInt Integer
              | LispSymbol String
              | LispFunc LispState FunctionSignature
              | LispQuot LispState FunctionSignature
              | LispList [LispExpr]

type FunctionSignature = [String]
type Context   = Map.Map String LispExpr
type LispError = ExceptT String IO
type LispState = StateT Context LispError LispExpr

instance Show LispExpr where
  show :: LispExpr -> String
  show (LispInt i)    = show i
  show (LispSymbol s) = s
  show (LispFunc _ sign) = "<function>" ++ show sign
  show (LispQuot _ sign) = "<special-form>" ++ show sign
  show (LispList xs) = "(" ++ unwords (show <$> xs) ++")"

eval :: LispExpr -> LispState
eval (LispInt i)        = return $ LispInt i
eval (LispFunc f sign)  = return $ LispFunc f sign
eval (LispQuot f sign)  = return $ LispQuot f sign
eval (LispSymbol s)     = flip (Map.!) s <$> get
eval (LispList (x:xs))  = eval x >>= apply where
  apply (LispQuot f expectedArgs) = apply' expectedArgs xs f
  apply (LispFunc f expectedArgs) = do
    args <- mapM eval xs
    apply' expectedArgs args f
  apply expr = throwError $ "[eval] " ++ show expr ++ " cannot call as function"
  apply' :: FunctionSignature -> [LispExpr] -> LispState -> LispState
  apply' expectedArgs args f = applyArgsToContext expectedArgs args >> f
  applyArgsToContext :: FunctionSignature -> [LispExpr] -> StateT Context LispError ()
  applyArgsToContext ("...":_) args = modify $ Map.insert "..." (LispList args)
  applyArgsToContext [] _ = return ()
  applyArgsToContext (earg:eargs) (arg:args) = do
    modify $ Map.insert earg arg
    applyArgsToContext eargs args
eval (LispList []) = throwError "[eval] Cannot eval empty list"

getSymbol :: String -> LispState
getSymbol symbol = do
  ctx <- get
  if symbol `Map.member` ctx
  then return $ ctx Map.! symbol
  else throwError $ "[getSymbol] symbol [" ++ symbol ++ "] NOT in context!"

getSymbols :: FunctionSignature -> StateT Context LispError [LispExpr]
getSymbols = mapM getSymbol
