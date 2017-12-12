module Main where

spreadsheet = "4347 3350 196 162 233 4932 4419 3485 4509 4287 4433 4033 207 3682 2193 4223\n\
              \648 94 778 957 1634 2885 1964 2929 2754 89 972 112 80 2819 543 2820\n\
              \400 133 1010 918 1154 1008 126 150 1118 117 148 463 141 940 1101 89\n\
              \596 527 224 382 511 565 284 121 643 139 625 335 657 134 125 152\n\
              \2069 1183 233 213 2192 193 2222 2130 2073 2262 1969 2159 2149 410 181 1924\n\
              \1610 128 1021 511 740 1384 459 224 183 266 152 1845 1423 230 1500 1381\n\
              \5454 3936 250 5125 244 720 5059 202 4877 5186 313 6125 172 727 1982 748\n\
              \3390 3440 220 228 195 4525 1759 1865 1483 5174 4897 4511 5663 4976 3850 199\n\
              \130 1733 238 1123 231 1347 241 291 1389 1392 269 1687 1359 1694 1629 1750\n\
              \1590 1394 101 434 1196 623 1033 78 890 1413 74 1274 1512 1043 1103 84\n\
              \203 236 3001 1664 195 4616 2466 4875 986 1681 152 3788 541 4447 4063 5366\n\
              \216 4134 255 235 1894 5454 1529 4735 4962 220 2011 2475 185 5060 4676 4089\n\
              \224 253 19 546 1134 3666 3532 207 210 3947 2290 3573 3808 1494 4308 4372\n\
              \134 130 2236 118 142 2350 3007 2495 2813 2833 2576 2704 169 2666 2267 850\n\
              \401 151 309 961 124 1027 1084 389 1150 166 1057 137 932 669 590 188\n\
              \784 232 363 316 336 666 711 430 192 867 628 57 222 575 622 234\n"

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
rowDividers row = head [a `div` b | a <- row, b <- row, a `mod` b == 0] --rowDividersRotate (length row) row

checksum :: String -> Int
checksum content = sum $ map (rowDiff . rowToInts) $ lines content
-- 47136

checksum2 :: String -> Int
checksum2 content = sum $ map rowDividers $ map rowToInts $ lines content
-- 250

--

main :: IO ()
main = putStrLn $ show $ checksum2 spreadsheet
