module Day03
  ( parts
  ) where

import Data.List
import Data.Ord (comparing)
import MyTuple (ap2, call2, dot2, map2, toList)

import AoC
import StringHelper

-- Split the list between 0s and 1s, compare the length of the
-- resulting sub-lists and return the head (can be any char really) of
-- the sub-list which satisfies the given "election" function.
commonality ::
     ((String -> String -> Ordering) -> [String] -> String) -> String -> Char
commonality electBy =
  head . electBy (comparing length) . toList . partition ((==) '0')

mostCommon :: String -> Char
mostCommon = commonality maximumBy

leastCommon :: String -> Char
leastCommon = commonality minimumBy

-- Get the relevant bit of the i-th column of the input, keep the
-- values whose i-th bit equals the relevant one, and keep going with
-- the next column, until there's only one vlaue left.
filter' :: (String -> Char) -> [String] -> Int -> String
filter' _ [x] _ = x
filter' fBit values i =
  let relevantBit = fBit $ (transpose values !! i)
      survivors = filter (\e -> (e !! i) == relevantBit) values
   in filter' fBit survivors (i + 1)

oxygenCriteria :: [String] -> String
oxygenCriteria values = filter' mostCommon values 0

co2Criteria :: [String] -> String
co2Criteria values = filter' leastCommon values 0

-- exports
part1 :: PartSolution
part1 input =
  let (g, e) =
        ap2 binaryToInt $
        map2 (mostCommon, leastCommon) $ transpose $ lines input
   in show $ g * e

part2 :: PartSolution
part2 input =
  let (o, c) =
        call2 (dot2 binaryToInt (oxygenCriteria, co2Criteria)) $ lines input
   in show $ o * c

parts :: DaySolutions
parts = (3, part1, part2)
