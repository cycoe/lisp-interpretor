module Libs.Expr where

import qualified Data.Map as Map
import Data.List (mapAccumL)
import Control.Monad.State (StateT, MonadState (get))

data LispExpr = LispInt Integer
              | LispSymbol String
              | LispFunc ([LispExpr] -> LispState)
              | LispQuot ([LispExpr] -> LispState)
              | LispList [LispExpr]

type Context   = Map.Map String LispExpr
type LispState = StateT Context IO LispExpr

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
