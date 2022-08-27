module Day06
  ( part1
  , part2
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import MyVector (VInt)

canHas :: Int -> Int -> VInt -> Int -> Bool
canHas start modulo banks e =
  let count_banks = length banks
      end_length = count_banks - start
      rest_end = min modulo end_length
      rest_beginning = modulo - rest_end
   in modulo > 0 &&
      ((e >= start && e < start + rest_end) || (e < rest_beginning))

spreadRest :: Int -> Int -> VInt -> VInt
spreadRest start rest banks =
  let count_banks = V.length banks
      next_bank = (start + 1) `mod` length banks
      spread_list =
        map (fromEnum . canHas next_bank rest banks) [0 .. count_banks - 1]
   in V.zipWith (+) (V.fromList spread_list) banks

emptyBank :: Int -> VInt -> VInt
emptyBank index banks = banks V.// [(index, 0)]

redistrib_ :: Int -> [VInt] -> VInt -> (Int, Int)
redistrib_ n old_banks banks =
  let bank = V.maxIndex banks
      count_blocks = banks V.! bank
      count_banks = length banks
      (d, m) = count_blocks `divMod` count_banks
      new_banks = spreadRest bank m $ V.map (+ d) $ emptyBank bank banks
   in if new_banks `elem` old_banks
        then (n, (+) 1 $ fromMaybe 0 $ elemIndex new_banks old_banks)
        else redistrib_ (n + 1) (new_banks : old_banks) new_banks

redistrib :: VInt -> (Int, Int)
redistrib l = redistrib_ 1 [l] l

countCycles :: ((Int, Int) -> Int) -> String -> String
countCycles pick = show . pick . redistrib . V.fromList . map read . words

-- exports
part1 :: String -> String
part1 = countCycles fst

part2 :: String -> String
part2 = countCycles snd
