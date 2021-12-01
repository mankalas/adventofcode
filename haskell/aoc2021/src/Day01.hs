module Day01
  ( part1
  , part2
  ) where

import AoC
import IntHelper
import MyList

-- Compare a pair of consecutive depths
depthIncrease :: (Int, Int) -> Bool
depthIncrease (a, b) = a < b

-- Compare a pair of depth windows by comparing their sum
depthWindowIncrease :: ((Int, Int, Int), (Int, Int, Int)) -> Bool
depthWindowIncrease (a, b) = (sum3 a) < (sum3 b)
  where
    sum3 (a, b, c) = a + b + c

-- Zip the original list with itself rotateted once and rotateted
-- twice, generating the sliding windows. We then compare the
-- consecutive windows.
depthWindowsIncreases :: [Int] -> Int
depthWindowsIncreases d1 =
  let d2 = rotate d1
      d3 = rotate d2
      d = zip3 d1 d2 d3
   in length $ filterConsecutive depthWindowIncrease d

-- exports
part1 :: Solution
part1 = show . length . filterConsecutive depthIncrease . linesToInts

part2 :: Solution
part2 = show . depthWindowsIncreases . linesToInts
