module Day01
  ( part1
  , part2
  ) where

import AoC
import IntHelper
import MyList

type Window = (Int, Int, Int)

-- Compare a pair of consecutive depths
depthIncrease :: (Int, Int) -> Bool
depthIncrease (a, b) = a < b

-- Compare a pair of depth windows by comparing their sum
windowIncrease :: (Window, Window) -> Bool
windowIncrease (a, b) = (sum3 a) < (sum3 b)
  where
    sum3 (a, b, c) = a + b + c

createWindows :: [Int] -> [Window]
createWindows d1 =
  let d2 = rotate d1
      d3 = rotate d2
   in zip3 d1 d2 d3

-- exports
part1 :: Solution
part1 = show . length . filterConsecutive depthIncrease . linesToInts

part2 :: Solution
part2 =
  show . length . filterConsecutive windowIncrease . createWindows . linesToInts
