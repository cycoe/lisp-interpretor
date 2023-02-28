
module Libs.Parser where

import Text.ParserCombinators.Parsec (GenParser, option, string, many1, many, digit,
                                      oneOf, letter, (<|>), try)
import Libs.Expr (LispExpr(..))

intP :: GenParser Char st LispExpr
intP = LispInt <$> do
  sign <- option "" (string "-")
  num <- many1 digit
  return . read $ sign ++ num

symbolP :: GenParser Char st LispExpr
symbolP = LispSymbol <$> do
  f <- firstAllowed
  r <- many $ firstAllowed <|> digit
  return (f:r) where
    firstAllowed = oneOf "+-*/" <|> letter

lispP :: GenParser Char st LispExpr
lispP = try intP <|> try symbolP
