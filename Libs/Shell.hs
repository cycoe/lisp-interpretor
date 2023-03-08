module Libs.Shell where

import System.Console.Haskeline (InputT, getInputLine, outputStrLn)
import Text.ParserCombinators.Parsec (runParser)
import Libs.Expr (Context, eval)
import Libs.Parser (lispP)
import Control.Monad.State (runStateT, liftIO)
import Control.Monad.Except (runExceptT)

shell :: Context -> InputT IO ()
shell ctx = do
  mline <- getInputLine "lisp> "
  case mline of
    Nothing   -> shell ctx
    Just line -> case runParser lispP "" "Lisp interpretor" line of
      Left error -> outputStrLn $ show error
      Right expr -> do
        result <- liftIO $ runExceptT (runStateT (eval expr) ctx)
        case result of
          Left error -> outputStrLn error >> shell ctx
          Right (expr', ctx') -> do
            outputStrLn (show (ctx', expr'))
            shell ctx'
