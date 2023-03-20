module Libs.Shell where

import System.Console.Haskeline (InputT, getInputLine, outputStrLn)
import Text.ParserCombinators.Parsec (runParser, GenParser, char, sepEndBy, many, many1, noneOf, spaces, unexpected, (<|>))
import Libs.Expr (Context, LispExpr, eval)
import Libs.Parser (lispP)
import Control.Monad.State (runStateT, liftIO)
import Control.Monad.Except (runExceptT)
import Libs.Builtin (symbols)

data ShellCommand = LoadSources [FilePath]
                  | RunLispExpr LispExpr
                  deriving Show

builtinP :: GenParser Char st ShellCommand
builtinP = do
  _ <- char ':'
  args <- sepEndBy (many1 $ noneOf " \t\v") spaces
  case args of
    ("l":paths) -> return $ LoadSources paths
    unhandled   -> unexpected $ show unhandled

shellP :: GenParser Char st ShellCommand
shellP = builtinP <|> RunLispExpr <$> lispP

loadSources :: Context -> [FilePath] -> InputT IO Context
loadSources ctx []     = return ctx
loadSources ctx (p:ps) = do
  source <- liftIO $ readFile p
  case runParser (many lispP) "" p source of
    Left err    -> outputStrLn (show err) >> return ctx
    Right exprs -> do
      result <- liftIO $ runExceptT (runStateT (mapM eval exprs) ctx)
      case result of
        Left err -> outputStrLn err >> return ctx
        Right (_, ctx') -> loadSources ctx' ps

shell :: Context -> InputT IO ()
shell ctx = do
  mline <- getInputLine "lisp> "
  case mline of
    Nothing   -> shell ctx
    Just line -> case runParser shellP "" "Lisp interpretor" line of
      Left err -> outputStrLn (show err) >> shell ctx
      Right cmd -> case cmd of
        LoadSources paths -> loadSources symbols paths >>= shell
        RunLispExpr expr -> do
          result <- liftIO $ runExceptT (runStateT (eval expr) ctx)
          case result of
            Left error -> outputStrLn error >> shell ctx
            Right (expr', ctx') -> do
              outputStrLn (show (ctx', expr'))
              shell ctx'
