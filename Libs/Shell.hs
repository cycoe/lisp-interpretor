module Libs.Shell where

import System.Console.Haskeline (InputT, getInputLine, outputStrLn)
import Text.ParserCombinators.Parsec (runParser)
import Libs.Expr (Context, eval)
import Libs.Parser (lispP)

shell :: Context -> InputT IO ()
shell ctx = do
  mline <- getInputLine "lisp> "
  case mline of
    Nothing   -> shell ctx
    Just line -> case runParser lispP "" "Lisp interpretor" line of
      Left error -> outputStrLn $ show error
      Right expr -> do
        outputStrLn (show (ctx', expr'))
        shell ctx' where
          (ctx', expr') = eval ctx expr
