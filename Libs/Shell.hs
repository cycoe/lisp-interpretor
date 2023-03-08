module Libs.Shell where

import System.Console.Haskeline (InputT, getInputLine, outputStrLn)
import Text.ParserCombinators.Parsec (runParser)
import Libs.Expr (Context, eval)
import Libs.Parser (lispP)
import Control.Monad.State (runStateT, liftIO)

shell :: Context -> InputT IO ()
shell ctx = do
  mline <- getInputLine "lisp> "
  case mline of
    Nothing   -> shell ctx
    Just line -> case runParser lispP "" "Lisp interpretor" line of
      Left error -> outputStrLn $ show error
      Right expr -> do
        (expr', ctx') <- liftIO $ runStateT (eval expr) ctx
        outputStrLn (show (ctx', expr'))
        shell ctx'
