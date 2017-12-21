module Day06 (part1, part2) where

import Common

import Data.List
import Data.Maybe
import qualified Data.Vector as V

can_has :: Int -> Int -> V.Vector (Int) -> Int -> Bool
can_has start mod banks e =
  let count_banks = length banks
      end_length = count_banks - start
      rest_end = min mod end_length
      rest_beginning = mod - rest_end in
    mod > 0 && ((e >= start && e < start + rest_end) || (e < rest_beginning))

spreadRest :: Int -> Int -> V.Vector (Int) -> V.Vector (Int)
spreadRest start rest banks =
  let count_banks = V.length banks
      next_bank = (start + 1) `mod` (length banks)
      spread_list = Prelude.map (fromEnum . can_has next_bank rest banks) [0..count_banks - 1] in
    V.zipWith (+) (V.fromList spread_list) banks

emptyBank :: Int -> V.Vector (Int) -> V.Vector (Int)
emptyBank index banks = banks V.// [(index, 0)]

redistrib_ :: Int -> [V.Vector (Int)] -> V.Vector (Int) -> (Int, Int)
redistrib_ n old_banks banks =
  let bank = V.maxIndex banks
      count_blocks = banks V.! bank
      count_banks = length banks
      (d, m) = count_blocks `divMod` count_banks
      new_banks = spreadRest bank m $ V.map (+ d) $ emptyBank bank banks in
    if new_banks `elem` old_banks
    then (n, (+) 1 $ fromMaybe 0 $ findIndex (\e -> e == new_banks) old_banks)
    else redistrib_ (n + 1) (new_banks:old_banks) new_banks

redistrib :: V.Vector (Int) -> (Int, Int)
redistrib l = redistrib_ 1 [l] l

-- exports

part1 :: String -> String
part1 input = show $ fst $ redistrib $ V.fromList $ map read $ words input

part2 :: String -> String
part2 input = show $ snd $ redistrib $ V.fromList $ map read $ words input
