module Day10(part1, part2) where

import Common

import Debug.Trace

import Data.List.Split
import qualified Data.Vector as V

knot_hash :: VInt -> VInt -> Int
knot_hash circle lengths =
  let c = knot_hash_ circle 0 0 lengths in
    c V.! 0 * c V.! 1

knot_hash_ :: VInt -> Int -> Int -> VInt -> VInt
knot_hash_ circle pos skip lengths
  | V.null lengths = circle
  | otherwise      =
    let l = V.head lengths
        ls = V.tail lengths
        new_pos = newPos pos l skip $ V.length circle in
        knot_hash_ (knot pos l circle) new_pos (skip + 1) ls

newPos :: Int -> Int -> Int -> Int -> Int
newPos cur_pos len skip list_len =
  (cur_pos + len + skip) `mod` list_len

knot :: Int -> Int -> VInt -> VInt
knot pos len circle = circularReverse circle pos len

-- export

part1 :: String -> String
part1 input = show $ knot_hash (V.fromList [0..255]) $ V.fromList $ map read $ splitOn "," input

part2 :: String -> String
part2 input = "12"
