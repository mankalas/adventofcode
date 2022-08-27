module Day10
  ( part1
  , part2
  , knotHash
  ) where

import Data.Bits (Bits(xor))
import Data.Char (chr, ord)
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Data.Vector.Split (chunksOf)
import Numeric (showHex)

import MyVector

type State = (VInt, Int, Int)

circle255 :: V.Vector Int
circle255 = V.fromList [0 .. 255]

lengthSuffix :: [Char]
lengthSuffix = map chr [17, 31, 73, 47, 23]

knot_hash :: VInt -> [Int] -> Int
knot_hash circle lengths =
  let (c, _, _) = sparseHash_ (circle, 0, 0) lengths
   in c V.! 0 * c V.! 1

sparseHash :: VInt -> [Int] -> VInt
sparseHash circle lengths =
  let (c, _, _) = iterate (`sparseHash_` lengths) (circle, 0, 0) !! 64
   in c

sparseHash_ :: State -> [Int] -> State
sparseHash_ (circle, pos, skip) lengths
  | null lengths = (circle, pos, skip)
  | otherwise =
    let l = head lengths
        ls = tail lengths
        l_circle = V.length circle
        new_pos = (pos + l + skip) `mod` l_circle
        reversed_circle = circularReverse circle pos l
     in sparseHash_ (reversed_circle, new_pos, skip + 1) ls

denseHash :: VInt -> [Int]
denseHash = map (foldl xor 0) . Data.Vector.Split.chunksOf 16

padHex :: Int -> String
padHex n =
  let x = showHex n ""
   in if length x == 1
        then "0" ++ x
        else x

finalHash :: [Int] -> String
finalHash = concatMap padHex

knotHash :: String -> String
knotHash input =
  finalHash $ denseHash $ sparseHash circle255 $ map ord (input ++ lengthSuffix)

-- export
part1 :: String -> String
part1 input =
  show $ knot_hash circle255 $ map read $ Data.List.Split.splitOn "," input

part2 :: String -> String
part2 = knotHash
