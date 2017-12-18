module Day04 where

import Data.List

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = (not $ elem x xs) && allUnique xs
-- 325

allNonAna :: [String] -> Bool
allNonAna [] = True
allNonAna l =
  let x:xs = map sort l in
    (not $ elem x xs) && allNonAna xs

countValid :: ([String] -> Bool) -> String -> Int
countValid valid input = length [ line | line <- lines input, valid $ words line]
