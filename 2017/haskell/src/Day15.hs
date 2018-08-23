module Day15 (part1, part2) where

import Common

import Debug.Trace

divisor :: Int
divisor = 2147483647

incrA :: Int -> Int
incrA = (*) 16807

incrB :: Int -> Int
incrB = (*) 48271

nextA :: Int -> Int
nextA x = (incrA x) `div` divisor

nextB :: Int -> Int
nextB x = (incrB x) `div` divisor





-- export

part1 :: String -> String
part1 = id

part2 :: String -> String
part2 = id
