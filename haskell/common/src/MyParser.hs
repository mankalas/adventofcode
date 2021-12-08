-- |
module MyParser where

import Text.Parsec
import Text.Parsec.String (Parser)

-- elements
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

number :: Parser Int
number =
  lexeme $ choice [char '-' *> fmap negate digits, char '+' *> digits, digits]
  where
    digits = read <$> many1 digit

symbol :: String -> Parser String
symbol = lexeme . try . string

-- parsing
parseWith :: Parser a -> String -> a
parseWith p = either (error . show) id . (parse p "(unknown)")

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith p s = parseWith p <$> lines s
