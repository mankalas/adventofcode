module Day01
  ( parts
  ) where

import AoC (DaySolutions, PartSolution)

step :: Char -> Int -> Int
step '(' i = i + 1
step ')' i = i - 1
step _ i = i

basement :: String -> Int -> Int -> Int
basement [] _ idx = idx
basement (x:xs) pos idx =
  if pos < 0
    then idx
    else basement xs (step x pos) (idx + 1)

-- exports
part1 :: PartSolution
part1 s = show $ foldr step 0 s

part2 :: PartSolution
part2 s = show $ basement s 0 0

parts :: DaySolutions
parts = (1, part1, part2)
