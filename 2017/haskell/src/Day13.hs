module Day13 (part1, part2) where

import Parser

import Data.List
import Data.Maybe

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

severity :: [Layer] -> Int
severity = foldl calcSeverity 0
  where calcSeverity n l@(range, depth)
          | depth == 0 = n
          | caught 0 l = n + range * depth
          | otherwise  = n

caught :: Int -> Layer -> Bool
caught delay (depth, range) =
  let divisor = 2 * (range - 1) in
    (delay + depth) `rem` divisor == 0

wait :: [Layer] -> Int
wait l = fromJust $ find (flip getThrough l) [0..]
  where getThrough n = not . any (caught n)

ridePacket f = show . f . parseInput firewall

-- export

part1 :: String -> String
part1 = ridePacket severity

part2 :: String -> String
part2 = ridePacket wait


