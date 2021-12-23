module Day03
  ( parts
  ) where

import Data.Containers.ListUtils
import Data.List
import Text.Parsec

import AoC
import Grid
import MyParser

parseDirections :: String -> [Direction]
parseDirections =
  parseWith $ do
    many
      ((char '^' >> return North) <|> (char 'v' >> return South) <|>
       (char '<' >> return West) <|>
       (char '>' >> return East))

-- exports
part1 :: PartSolution
part1 s =
  let directions = parseDirections s
      (_, houses) = mapAccumL (\pos dir -> (go pos dir, pos)) (0, 0) directions
   in show $ length $ nubOrd houses

part2 :: PartSolution
part2 s = ""

parts :: DaySolutions
parts = (3, part1, part2)
