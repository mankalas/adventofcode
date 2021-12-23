module Day04
  ( parts
  ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust)

import AoC (DaySolutions, PartSolution)

nugget :: String -> String -> Int -> Bool
nugget p s i = isPrefixOf p $ show $ md5 $ pack (s ++ show i)

mine :: String -> String -> String
mine prefix s = show $ fromJust $ find (nugget prefix s) [1 ..]

-- exports
part1 :: PartSolution
part1 = mine "00000"

part2 :: PartSolution
part2 = mine "000000"

parts :: DaySolutions
parts = (4, part1, part2)
