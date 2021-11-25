module Common where

import Debug.Trace

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type VInt = V.Vector (Int)

type Solution = String -> String

rowToInts :: String -> [Int]
rowToInts = map read . words

increment :: Int -> Int -> VInt -> VInt
increment pos incr v = v V.// [(pos, v V.! pos + incr)]

circularReverse :: VInt -> Int -> Int -> VInt
circularReverse v p l
  | p + l > V.length v = circularReverse_ v p l
  | otherwise = simpleReverse v p l

circularReverse_ :: VInt -> Int -> Int -> VInt
circularReverse_ v p l =
  let lv = length v
      remainder = lv - p
      overflow = l - remainder
      id_v = V.take (lv - remainder - overflow) $ V.drop overflow v
      sub_v = V.drop p v V.++ V.take overflow v
      rev_v = V.reverse sub_v
      (rev_end, rev_beg) = V.splitAt remainder rev_v
   in rev_beg V.++ id_v V.++ rev_end

simpleReverse :: VInt -> Int -> Int -> VInt
simpleReverse v p l =
  let (v1, rv) = V.splitAt p v
      (v2, v3) = V.splitAt l rv
   in v1 V.++ V.reverse v2 V.++ v3

-- Grid
type Coord = (Int, Int)

up (x, y) = (x, y + 1)

down (x, y) = (x, y - 1)

left (x, y) = (x - 1, y)

right (x, y) = (x + 1, y)

north p = M.lookup (up p)

south p = M.lookup (down p)

west p = M.lookup (left p)

east p = M.lookup (right p)

cardinalPoints :: Coord -> M.Map Coord a -> [Maybe a]
cardinalPoints c grid =
  let neighbors = map (\f -> f c) [up, down, left, right]
   in map (\n -> M.lookup n grid) neighbors
