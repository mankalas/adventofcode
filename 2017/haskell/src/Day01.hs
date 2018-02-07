module Day01 (part1, part2) where

import Common

import Data.Char

-- exports

part1 :: String -> String
part1 l1@(x:xs) = show $ sum [ digitToInt a | (a, b) <- zip l1 l2, a == b ]
  where l2 = xs ++ [x]

part2 :: String -> String
part2 l1 = show $ sum [ 2 * digitToInt a | (a, b) <- zip l1 l2, a == b ]
  where l2 = drop (length l1 `div` 2) l1
