module Day03
  ( parts
  ) where

import Data.List

import AoC
import StringHelper

gammaCriteria :: String -> Char
gammaCriteria l =
  let (zeros, ones) = partition ((==) '0') l
   in if (length zeros) > (length ones)
        then '0'
        else '1'

espilonCriteria :: String -> Char
espilonCriteria l =
  let (zeros, ones) = partition ((==) '0') l
   in if (length zeros) > (length ones)
        then '1'
        else '0'

-- exports
part1 :: PartSolution
part1 input =
  let nthBits = transpose $ lines input
      gamma = rating gammaCriteria nthBits
      epsilon = rating espilonCriteria nthBits
   in show $ gamma * epsilon
  where
    rating f = binaryToInt . map f

part2 :: PartSolution
part2 = id

parts :: DaySolutions
parts = (3, part1, part2)
