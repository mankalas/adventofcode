module Day04 (part1, part2) where

import Common

import Data.List
import Data.List.Unique

-- code

validate :: ([String] -> Bool) -> String -> String
validate policy input = show $ length [ line | line <- lines input, policy $ words line]

-- exports

part1 :: String -> String
part1 = validate allUnique

part2 :: String -> String
part2 = validate (allUnique . map sort)
