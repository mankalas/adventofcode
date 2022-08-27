-- |
module MyVector where

import qualified Data.Vector as V

type VInt = V.Vector Int

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
