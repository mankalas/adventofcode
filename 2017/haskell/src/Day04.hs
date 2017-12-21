module Day04 (part1, part2) where

import Common

import Data.List
import Data.List.Unique

-- exports

part1 :: String -> String
part1 input = show $ length [ line | line <- lines input, allUnique $ words line]

part2 :: String -> String
part2 input = show $ length [ line | line <- lines input, allUnique $ map sort $ words line]
