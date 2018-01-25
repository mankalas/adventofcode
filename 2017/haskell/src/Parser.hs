module Parser where

import Text.ParserCombinators.Parsec

import Control.Monad (ap)

int :: GenParser Char st Int
int = ap sign nat

sign :: Num a => CharParser st (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

nat :: CharParser st Int
nat =
  do { char '0' >> return 0 } <|>
  do { n <- many1 digit; return (read n) }


