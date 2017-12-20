module Day01 where

import Data.Char

sumCaptcha :: String -> Int
sumCaptcha [] = 0
sumCaptcha [x] = digitToInt x
sumCaptcha (x1:x2:xs) =
  let increment = if x1 == x2 then digitToInt x1 else 0 in
    increment + sumCaptcha (x2:xs)

validDigit2 :: String -> Bool
validDigit2 string =
  let half_length = (length string) `div` 2
      other_char = string !! half_length in
      head string == other_char

rotate :: String -> String
rotate [] = []
rotate (x:xs) = xs ++ [x]

sumCaptchaRotate :: Int -> String -> Int
sumCaptchaRotate 0 _ = 0
sumCaptchaRotate n string =
  let increment = if validDigit2 string
                  then digitToInt $ head string
                  else 0 in
    increment + sumCaptchaRotate (n - 1) (rotate string)

sumCaptcha2 :: String -> Int
sumCaptcha2 string = sumCaptchaRotate (length string) string

-- exports

part1 :: String -> String
part1 input = show $ sumCaptcha input

part2 :: String -> String
part2 input = show $ sumCaptcha2 input
