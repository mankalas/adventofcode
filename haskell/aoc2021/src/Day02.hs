module Day02
  ( parts
  ) where

import AoC
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Control.Monad (ap)

data Command
  = Forward Int
  | Down Int
  | Up Int

type Aim = Int

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

commands :: Parser [Command]
commands = do
  result <- many command
  eof
  return result

applyCmd :: Position -> Command -> Position
applyCmd (h, d) cmd =
  case cmd of
    Forward i -> (h + i, d)
    Up i -> (h, d - i)
    Down i -> (h, d + i)

applyCmdAim :: (Position, Aim) -> Command -> (Position, Aim)
applyCmdAim ((h, d), a) cmd =
  case cmd of
    Forward i -> ((h + i, d + a * i), a)
    Up i -> ((h, d), a - i)
    Down i -> ((h, d), a + i)

navigate :: [Command] -> Position
navigate = foldl applyCmd (0, 0)

navigateWithAim :: [Command] -> Position
navigateWithAim cmds =
  let (p, _) = foldl applyCmdAim ((0, 0), 0) cmds
   in p

parseInput :: GenParser Char () a -> String -> a
parseInput p = (either (error . show) id) . (parse p "(unknown)")

-- exports
part1 :: PartSolution
part1 input =
  let (h, d) = navigate $ parseInput commands input
   in show $ h * d

part2 :: PartSolution
part2 input =
  let (h, d) = navigateWithAim $ parseInput commands input
   in show $ h * d

parts :: DaySolutions
parts = (2, part1, part2)
