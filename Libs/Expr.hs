module Libs.Expr where

import qualified Data.Map as Map
import Data.List (mapAccumL)
import Control.Monad.State (StateT, MonadState (get))
import Control.Monad.Except (ExceptT, MonadError (throwError))

data LispExpr = LispInt Integer
              | LispSymbol String
              | LispFunc ([LispExpr] -> LispState)
              | LispQuot ([LispExpr] -> LispState)
              | LispList [LispExpr]

type Context   = Map.Map String LispExpr
type LispError = ExceptT String IO
type LispState = StateT Context LispError LispExpr

instance Show LispExpr where
  show :: LispExpr -> String
  show (LispInt i)    = show i
  show (LispSymbol s) = s
  show (LispFunc _) = "<function>"
  show (LispQuot _) = "<special-form>"
  show (LispList xs) = "(" ++ unwords (show <$> xs) ++")"

eval :: LispExpr -> LispState
eval (LispInt i)        = return $ LispInt i
eval (LispFunc f)       = return $ LispFunc f
eval (LispQuot f)       = return $ LispQuot f
eval (LispSymbol s)     = flip (Map.!) s <$> get
eval (LispList (x:xs))  = do
  fn <- eval x
  apply fn where
    apply (LispQuot f) = f xs
    apply (LispFunc f) = mapM eval xs >>= f
    apply expr         = throwError $ "[eval] " ++ show expr ++ " cannot call as function"
eval (LispList []) = throwError "[eval] Cannot eval empty list"
