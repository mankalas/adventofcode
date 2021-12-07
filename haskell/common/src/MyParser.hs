-- |
module MyParser where

import Control.Monad (ap)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

int :: GenParser Char st Int
int = ap sign nat

sign :: Num a => CharParser st (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

nat :: CharParser st Int
nat =
  do char '0' >> return 0
     <|> do
    n <- many1 digit
    return (read n)

ints :: CharParser st [Int]
ints = int `sepBy` (char ',')

parseInput :: CharParser () a -> String -> a
parseInput p = (either (error . show) id) . (parse p "(unknown)")
