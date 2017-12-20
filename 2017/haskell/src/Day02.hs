module Day02 where

rowToInts :: String -> [Int]
rowToInts row = map read $ words row

rowDiff :: [Int] -> Int
rowDiff ints = maximum ints - minimum ints

dividerCheck :: Int -> [Int] -> Int
dividerCheck _ [] = 0
dividerCheck n (x:xs) =
  let (div, rem) = quotRem n x in
    if rem == 0
    then div
    else dividerCheck n xs

rowDividersRotate :: Int -> [Int] -> Int
rowDividersRotate 0 _ = 0
rowDividersRotate n (x:xs) =
  let result = dividerCheck x xs in
    if result == 0
    then rowDividersRotate (n - 1) (rotate (x:xs))
    else result

rotate :: [a] -> [a]
rotate list = zipWith const (drop 1 (cycle list)) list

rowDividers :: [Int] -> Int
rowDividers row = rowDividersRotate (length row) row --head [a `div` b | a <- row, b <- row, a `mod` b == 0] --

checksum :: String -> Int
checksum content = sum $ map (rowDiff . rowToInts) $ lines content

checksum2 :: String -> Int
checksum2 content = sum $ map rowDividers $ map rowToInts $ lines content

-- exports

part1 :: String -> String
part1 input = show $ checksum input

part2 :: String -> String
part2 input = show $ checksum2 input
