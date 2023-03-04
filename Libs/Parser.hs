
module Libs.Parser where

import Text.ParserCombinators.Parsec (GenParser, option, string, many1, many, digit,
                                      oneOf, letter, (<|>), try, char, sepBy, spaces)
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

listP :: GenParser Char st LispExpr
listP = LispList <$> do
  char '(' *> sepBy lispP spaces <* char ')'

lispP :: GenParser Char st LispExpr
lispP = try intP <|> try symbolP <|> listP
