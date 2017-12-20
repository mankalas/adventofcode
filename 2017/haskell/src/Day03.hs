module Day03 where

import Data.Maybe
import Data.List
import Data.List.Split

whichSquare :: Int -> [Int]
whichSquare n = fromJust $ find (n `elem`) squares
  where squares = [[1]] ++ splitPlaces (map (8*) [1..]) [2..]

steps :: [Int] -> [(Int, Int)]
steps sq =
  let l = length sq
      n = l `div` 8
      m = n * 2
      sq2 = last sq : init sq
      nm = [n..m]
      mn = reverse nm
      steps = take l $ cycle $ init $ mn ++ tail nm in
    zip sq2 steps

ans :: Int -> Int
ans n =
  let tuples = steps $ whichSquare n
      tuple = fromJust $ find ((== n) . fst) tuples in
    snd tuple

-- exports

part1 :: String -> String
part1 input = show $ ans $ read input

part2 :: String -> String
part2 input = show 0
