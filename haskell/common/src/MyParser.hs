-- |
module MyParser where

import Text.ParserCombinators.Parsec

parseInput :: GenParser Char () a -> String -> a
parseInput p = (either (error . show) id) . (parse p "(unknown)")
