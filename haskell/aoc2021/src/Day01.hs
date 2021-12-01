module Day01
  ( part1
  , part2
  ) where

linesToInts :: String -> [Int]
linesToInts = map read . lines

-- Rotate a list by concatenating its tail with its head
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

-- Compare a pair of consecutive depths
depthIncrease :: (Int, Int) -> Bool
depthIncrease (a, b) = a < b

-- Compare a pair of depth windows by comparing their sum
depthWindowIncrease :: ((Int, Int, Int), (Int, Int, Int)) -> Bool
depthWindowIncrease (a, b) = (sum3 a) < (sum3 b)
  where
    sum3 (a, b, c) = a + b + c

-- Count the number of consecutive values in a list that satisfy a
-- predicate
countConsecutiveIncreases :: ((a, a) -> Bool) -> [a] -> Int
countConsecutiveIncreases predicate l1 =
  let l2 = rotate l1
      l = zip l1 l2
   in length $ filter predicate l

-- Zip the original list with itself rotateted once and rotateted
-- twice, generating the sliding windows. We then compare the
-- consecutive windows.
depthWindowsIncreases :: [Int] -> Int
depthWindowsIncreases d1 =
  let d2 = rotate d1
      d3 = rotate d2
      d = zip3 d1 d2 d3
   in countConsecutiveIncreases depthWindowIncrease d

-- exports
part1 :: String -> String
part1 = show . countConsecutiveIncreases depthIncrease . linesToInts

part2 :: String -> String
part2 = show . depthWindowsIncreases . linesToInts
