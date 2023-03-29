
module Libs.Parser where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (GenParser, option, string, many1, many, digit,
                                      oneOf, letter, (<|>), try, char, sepBy, spaces, anyChar, satisfy, noneOf, manyTill)
import Libs.Expr (LispExpr(..))

charEscapeMap :: Map.Map Char Char
charEscapeMap = Map.fromList
  [ ('n', '\n')
  , ('r', '\r')
  , ('v', '\v')
  , ('b', '\b')
  , ('t', '\t')
  , ('\'', '\'')
  , ('"', '"')
  , ('\\', '\\')
  ]

intP :: GenParser Char st LispExpr
intP = LispInt <$> do
  sign <- option "" (string "-")
  num <- many1 digit
  return . read $ sign ++ num

escapeP :: GenParser Char st Char
escapeP = (Map.!) charEscapeMap <$> satisfy (`Map.member` charEscapeMap)

validCharP :: GenParser Char st Char
validCharP = noneOf "\\" <|> char '\\' *> escapeP

charP :: GenParser Char st LispExpr
charP = LispChar <$> (char '\'' *> validCharP <* char '\'')

stringP :: GenParser Char st LispExpr
stringP = LispString <$> (char '"' *> manyTill validCharP (char '"'))

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
lispP = try intP <|> try charP <|> try stringP <|> try symbolP <|> listP
