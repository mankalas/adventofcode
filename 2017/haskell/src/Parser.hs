module Parser where

import Data.Either

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

type LeafDef = (String, Int, [String])

file :: GenParser Char st [LeafDef]
file = do
  result <- many line
  eof
  return result

line :: GenParser Char st LeafDef
line = do
  n <- many alphaNum
  _ <- char ' '
  w <- weight
  c <- children
  _ <- endOfLine
  return (n, w, c)

weight :: GenParser Char st Int
weight = do
  _ <- char '('
  w <- many digit
  _ <- char ')'
  return $ read w

children :: GenParser Char st [String]
children = do
  first <- child
  next <- remainingChildren
  if first == "" then return [] else return (first:next)

child :: GenParser Char st String
child = try ( do
                _ <- string " -> "
                c <- many (noneOf ",\n")
                return c
            ) <|> many (noneOf ",\n")

remainingChildren :: GenParser Char st [String]
remainingChildren = (string ", " >> children) <|> return []

parseFile :: String -> [LeafDef]
parseFile input =
  let result = parse file "(unknown)" input in
    if isLeft result
    then error("parse error")
    else fromRight [] result

