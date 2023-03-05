module Libs.Expr where

import qualified Data.Map as Map
import Data.List (mapAccumL)

data LispExpr = LispInt Integer
              | LispSymbol String
              | LispFunc (Context -> [LispExpr] -> (Context, LispExpr))
              | LispQuot (Context -> [LispExpr] -> (Context, LispExpr))
              | LispList [LispExpr]

type Context = Map.Map String LispExpr

instance Show LispExpr where
  show :: LispExpr -> String
  show (LispInt i)    = show i
  show (LispSymbol s) = s
  show (LispFunc _) = "<function>"
  show (LispQuot _) = "<special-form>"
  show (LispList xs) = "(" ++ unwords (show <$> xs) ++")"

eval :: Context -> LispExpr -> (Context, LispExpr)
eval ctx (LispInt i)        = (ctx, LispInt i)
eval ctx (LispSymbol s)     = (ctx, ctx Map.! s)
eval ctx (LispFunc f)       = (ctx, LispFunc f)
eval ctx (LispQuot f)       = (ctx, LispQuot f)
eval ctx (LispList (x:xs))  =
  let (new_ctx, fn) = eval ctx x
      (last_ctx, eval_args) = mapAccumL eval new_ctx xs
      apply (LispFunc f) = f last_ctx eval_args
      apply (LispQuot f) = f new_ctx xs
      apply _            = undefined
  in  apply fn
