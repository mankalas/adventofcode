module Day01
  ( parts
  ) where

import AoC

step :: Char -> Int -> Int
step '(' i = i + 1
step ')' i = i - 1
step _ i = i

-- exports
part1 :: PartSolution
part1 s = show $ foldr step 0 s

part2 :: PartSolution
part2 s = ""

parts :: DaySolutions
parts = (1, part1, part2)
