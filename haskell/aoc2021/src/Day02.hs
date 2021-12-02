module Day02
  ( part1
  , part2
  ) where

import AoC
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Control.Monad (ap)

data Command
  = Forward Int
  | Down Int
  | Up Int

type Position = (Int, Int)

num :: Parser Integer
num = do
  n <- many1 digit
  return (read n)

forward :: Parser Command
forward = do
  n <- string "forward"
  _ <- char ' '
  i <- many1 digit
  return (Forward (read i))

up :: Parser Command
up = do
  n <- string "up"
  _ <- char ' '
  i <- many1 digit
  return (Up (read i))

down :: Parser Command
down = do
  n <- string "down"
  _ <- char ' '
  i <- many1 digit
  return (Down (read i))

command :: Parser Command
command = do
  c <- forward <|> up <|> down
  _ <- char '\n'
  return c

lines :: Parser [Command]
lines = do
  result <- many command
  eof
  return result

apply :: Command -> Position -> Position
apply cmd (h, d) =
  case cmd of
    Forward i -> (h + i, d)
    Up i -> (h, d - i)
    Down i -> (h, d + i)

navigate :: [Command] -> Position
navigate list = apply (head list) (0, 0)

showCmd :: Command -> String
showCmd cmd = "#"
  -- case cmd of
  --   Forward i -> "Fwd" ++ show i
  --   Up i -> "Up" ++ show i
  --   Down i -> "Down" ++ show i

showCmds :: [Command] -> String
showCmds = concat . map showCmd

parseInput :: GenParser Char () a -> String -> a
parseInput p = (either (error . show) id) . (parse p "(unknown)")

-- exports
part1 :: Solution
part1 input =
  let cmds = parseInput Day02.lines input
      (a, b) = navigate cmds
      result = showCmds cmds
   -- in result    -- works: display plenty of '#'
   in show $ a + b -- doesn't work: errors with 'Prelude.head: empty list'...

part2 :: Solution
part2 = id
