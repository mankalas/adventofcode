module Main where

import Data.List
import Data.Maybe
import Debug.Trace

select :: [Int] -> Int
select l =
  let max = maximum l
      index = findIndex (\e -> e == max) l in
    fromMaybe 0 index

increment :: Int -> Int -> [Int] -> [Int]
increment pos offset list =
  let (a, z) = splitAt pos list in a ++ (head z + offset : tail z)

can_has :: Int -> Int -> [Int] -> Int -> Bool
can_has start mod banks e =
  if mod <= 0
  then False
  else
    let count_banks = length banks
        end_length = count_banks - start
        rest_end = min mod end_length
        rest_beginning = mod - rest_end in
      (e >= start && e < start + rest_end) || (e < rest_beginning)

boolToInt True = 1
boolToInt False = 0

nextBank n banks = (n + 1) `mod` (length banks)

spreadRest :: Int -> Int -> [Int] -> [Int]
spreadRest start 1 banks = increment start 1 banks
spreadRest start mod banks =
  let count_banks = length banks
      next_bank = nextBank start banks
      spread_list = map (boolToInt . can_has next_bank mod banks) [0..count_banks - 1] in
    zipWith (+) spread_list banks

emptyBank :: Int -> [Int] -> [Int]
emptyBank index banks =
  let (a,z) = splitAt index banks in
    a ++ (0:tail z)

redistrib_ :: Int -> [[Int]] -> [Int] -> Int
redistrib_ n old_banks banks =
  let bank = trace("Banks: " ++ show banks) select banks
      count_blocks = trace("Selected bank: " ++ show bank) banks !! bank
      count_banks = length banks
      (d, m) = count_blocks `divMod` count_banks
      new_banks = spreadRest bank m $ map (+ d) $ emptyBank bank banks in
    if new_banks `elem` old_banks
    then n
    else redistrib_ (n + 1) (new_banks:old_banks) new_banks

redistrib :: [Int] -> Int
redistrib l = redistrib_ 1 [l] l

input = "11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11"
test_input = "0 2 7 0" -- works

main :: IO ()
main = putStrLn $ show $ redistrib $ map read $ words input
