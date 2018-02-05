module Day13 (part1, part2) where

import Parser

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

type Layer = (Int, Int)

firewall :: GenParser Char () [Layer]
firewall = do
  l <- many layer
  eof
  return l

layer :: GenParser Char () Layer
layer = do
  depth <- int
  string ": "
  range <- int
  endOfLine
  return (depth, range)

patrol :: Int -> [Int]
patrol n = cycle $ [0..n] ++ reverse [1..n-1]

severity :: [Layer] -> Int
severity l = severity_ l 0 0 0

severity_ :: [Layer] -> Int -> Int -> Int -> Int
severity_ [] s _ _ = s
severity_ l@(x:xs) s p o =
  let (depth, range) = x
      incr_sev = range * depth + if depth == 0 then o else 0 in
    if p < depth
    then severity_ l s (p + 1) o
    else
      if (patrol $ max (range - 1) 0) !! p == 0
      then severity_ xs (s + range * depth) (p + 1) o
      else severity_ xs s (p + 1) o

pass :: [Layer] -> Int
pass l = pass_ l 1
  where pass_ l@(x:xs) d =


-- export

part1 :: String -> String
part1 input = show $ severity $ parseInput firewall input

part2 :: String -> String
part2 input = show $ pass $ parseInput firewall input -- 48 wrong


