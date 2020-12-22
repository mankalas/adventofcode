module Day11 (part1, part2) where

import Data.List.Split

type Coord = (Int, Int)

walk :: Coord -> String -> Coord
walk (x, y) s
  | s == "n"  = (x,     y - 1)
  | s == "ne" = (x + 1, y - 1)
  | s == "se" = (x + 1, y)
  | s == "s"  = (x,     y + 1)
  | s == "sw" = (x - 1, y + 1)
  | s == "nw" = (x - 1, y)

walkWithMax :: (Coord, Int) -> String -> (Coord, Int)
walkWithMax (c, m) s =
  let next_step = walk c s
      d = dist next_step in
    (next_step, max d m)

dist :: Coord -> Int
dist (x, y) = (abs x + abs (x + y) + abs y) `div` 2

-- export

part1 :: String -> String
part1 input = show $ dist $ foldl walk (0, 0) $ splitOn "," input

part2 :: String -> String
part2 input = show $ snd $ foldl walkWithMax ((0, 0), 0) $ splitOn "," input
