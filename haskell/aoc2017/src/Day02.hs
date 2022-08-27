module Day02
  ( part1
  , part2
  ) where

import IntHelper

type Calculation = [Int] -> Int

deviation :: Calculation
deviation l = maximum l - minimum l

division :: Calculation
division row = head [a `div` b | a <- row, b <- row, a `mod` b == 0 && a /= b]

checksum :: Calculation -> String -> Int
checksum f = sum . map (f . rowToInts) . lines

-- exports
part1 :: String -> String
part1 = show . checksum deviation

part2 :: String -> String
part2 = show . checksum division
