module Day02
  ( parts
  ) where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import AoC
import MyParser

data Command
  = Forward Int
  | Down Int
  | Up Int

type Aim = Int

type Position = (Int, Int)

forward :: Parser Command
forward = do
  _ <- string "forward "
  i <- many1 digit
  return (Forward (read i))

up :: Parser Command
up = do
  _ <- string "up "
  i <- many1 digit
  return (Up (read i))

down :: Parser Command
down = do
  _ <- string "down "
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
