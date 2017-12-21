module Day02 (part1, part2) where

import Common

deviation :: [Int] -> Int
deviation l = maximum l - minimum l

division :: [Int] -> Int
division row = head [a `div` b | a <- row, b <- row, a `mod` b == 0 && a /= b]

answer :: String -> ([Int] -> Int) -> Int
answer input f = sum $ map (f . rowToInts) $ lines input

-- exports

part1 :: String -> String
part1 input = show $ answer input deviation

part2 :: String -> String
part2 input = show $ answer input division
