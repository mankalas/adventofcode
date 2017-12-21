module Day05 (part1, part2) where

import Common

import Data.Vector as V

plus1 :: Int -> Int
plus1 _ = 1

pom :: Int -> Int
pom n | n >= 3 = -1
pom _ = 1

move :: Int -> Int -> (Int -> Int) -> V.Vector (Int) -> Int
move pos n f v =
  let offset = v ! pos
      next_pos = pos + offset in
    if next_pos < 0 || next_pos >= V.length v
    then n + 1
    else
      let incr = f offset in move next_pos (n + 1) f (increment pos incr v)

-- exports

part1 :: String -> String
part1 input = show $ move 0 0 plus1 $ V.fromList (Prelude.map read $ lines input)

part2 :: String -> String
part2 input = show $ move 0 0 pom $ V.fromList (Prelude.map read $ lines input)
