module Day13
  ( part1
  , part2
  ) where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String (Parser)

import MyParser

type Layer = (Int, Int)

firewall :: Parser [Layer]
firewall = do
  l <- many layer
  eof
  return l

layer :: Parser Layer
layer = do
  depth <- number
  string ": "
  range <- number
  endOfLine
  return (depth, range)

severity :: [Layer] -> Int
severity = foldl calcSeverity 0
  where
    calcSeverity n l@(range, depth)
      | depth == 0 = n
      | caught 0 l = n + range * depth
      | otherwise = n

caught :: Int -> Layer -> Bool
caught delay (depth, range) =
  let divisor = 2 * (range - 1)
   in (delay + depth) `rem` divisor == 0

wait :: [Layer] -> Int
wait l = fromJust $ find (`getThrough` l) [0 ..]
  where
    getThrough n = not . any (caught n)

ridePacket :: Show a => ([Layer] -> a) -> String -> String
ridePacket f = show . f . parseWith firewall

-- export
part1 :: String -> String
part1 = ridePacket severity

part2 :: String -> String
part2 = ridePacket wait
