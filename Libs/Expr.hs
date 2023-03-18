{-# LANGUAGE InstanceSigs #-}
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
type SymbolTable = Map.Map String LispExpr
data Context     = Context SymbolTable (Maybe Context) deriving Show
type LispError   = ExceptT String IO
type LispState   = StateT Context LispError LispExpr

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
eval (LispSymbol s)     = getSymbol s
eval (LispList (x:xs))  = eval x >>= apply where
  apply (LispQuot f expectedArgs) = apply' expectedArgs xs f
  apply (LispFunc f expectedArgs) = do
    args <- mapM eval xs
    apply' expectedArgs args f
  apply expr = throwError $ "[eval] " ++ show expr ++ " cannot call as function"
  apply' :: FunctionSignature -> [LispExpr] -> LispState -> LispState
  apply' expectedArgs args f = do
    modify pushContext
    applyArgsToContext expectedArgs args
    result <- f
    modify popContext
    return result
  applyArgsToContext :: FunctionSignature -> [LispExpr] -> StateT Context LispError ()
  applyArgsToContext ("...":_) args = updateSymbol "..." $ LispList args
  applyArgsToContext [] _ = return ()
  applyArgsToContext (earg:eargs) (arg:args) = do
    updateSymbol earg arg
    applyArgsToContext eargs args
eval (LispList []) = throwError "[eval] Cannot eval empty list"

pushContext :: Context -> Context
pushContext ctx = Context Map.empty (Just ctx)

popContext :: Context -> Context
popContext (Context _ (Just parent)) = parent
popContext ctx@(Context _ Nothing) = ctx

getSymbol :: String -> LispState
getSymbol symbol = do
  ctx <- get
  getSymbolFrom symbol ctx where
    getSymbolFrom :: String -> Context -> LispState
    getSymbolFrom symbol (Context st mp) =
      if symbol `Map.member` st
      then return $ st Map.! symbol
      else case mp of
        Nothing -> throwError $ "[getSymbol] symbol [" ++ symbol ++ "] NOT in symbol table!"
        Just parent -> getSymbolFrom symbol parent

getSymbols :: FunctionSignature -> StateT Context LispError [LispExpr]
getSymbols = mapM getSymbol

updateSymbol :: String -> LispExpr -> StateT Context LispError ()
updateSymbol symbol expr = modify $ \(Context st mp) -> Context (Map.insert symbol expr st) mp

updateSymbolInParent :: String -> LispExpr -> StateT Context LispError ()
updateSymbolInParent symbol expr = modify $ \(Context st mp) -> Context st (update mp) where
  update (Just (Context st mp)) = Just (Context (Map.insert symbol expr st) mp)
