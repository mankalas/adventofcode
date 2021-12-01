-- |
module IntHelper where

linesToInts :: String -> [Int]
linesToInts = map read . lines

rowToInts :: String -> [Int]
rowToInts = map read . words
