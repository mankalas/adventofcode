module Day10(part1, part2) where

import Common

import Debug.Trace

import Text.Printf
import Numeric
import Data.Char
import Data.Bits
import qualified Data.Vector as V
import Data.List.Split
import Data.Vector.Split

type State = (VInt, Int, Int)

circle255 = V.fromList [0..255]
length_suffix = [17, 31, 73, 47, 23]

knot_hash :: VInt -> [Int] -> Int
knot_hash circle lengths =
  let (c, _, _) = sparseHash_ (circle, 0, 0) lengths in
    c V.! 0 * c V.! 1

sparseHash :: VInt -> [Int] -> VInt
sparseHash circle lengths =
  let (c, _, _) = iterate (\s -> sparseHash_ s lengths) (circle, 0, 0) !! 64 in c

sparseHash_ :: State -> [Int] -> State
sparseHash_ (circle, pos, skip) lengths
  | null lengths = (circle, pos, skip)
  | otherwise    =
      let l = head lengths
          ls = tail lengths
          l_circle = V.length circle
          new_pos = (pos + l + skip) `mod` l_circle
          reversed_circle = circularReverse circle pos l in
        sparseHash_ (reversed_circle, new_pos, (skip + 1)) ls

denseHash :: VInt -> [Int]
denseHash = map (foldl xor 0) . Data.Vector.Split.chunksOf 16

padHex :: Int -> String
padHex n =
  let x = showHex n "" in
    if length x == 1 then "0" ++ x else x

finalHash :: [Int] -> String
finalHash = (foldl (++) "") . map padHex

-- export

part1 :: String -> String
part1 input = show $ knot_hash circle255 $ map read $ Data.List.Split.splitOn "," input

part2 :: String -> String
part2 input = finalHash $ denseHash $ sparseHash circle255 $ map ord input ++ length_suffix
