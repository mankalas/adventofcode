module Day15 (part1, part2) where

import Common

import Debug.Trace

type Increment = Int -> Int

divisor = 2147483647
sampleA = 40000000
sampleB = 5000000
incrA = (*) 16807
incrB = (*) 48271
nextA x = (incrA x) `mod` divisor
nextB x = (incrB x) `mod` divisor

nextA2 x =
  let n = nextA x in
    if n `mod` 4 == 0
    then n
    else nextA2 n

nextB2 x =
  let n = nextB x in
    if n `mod` 8 == 0
    then n
    else nextB2 n

pad :: Int -> String -> String
pad n s = replicate (n - length s) '0' ++ s

toBin :: Int -> String
toBin 0 = ""
toBin x =
  let (q, r) = x `divMod` 2 in
    toBin q ++ show r

hash :: Int -> String
hash = drop 16 . pad 32 . toBin

cmp :: Int -> Int -> Bool
cmp a b =
  (hash a) == (hash b)

count :: Int -> Int -> Increment -> Increment -> Int -> Int
count a b next_a next_b sample = count_ a b next_a next_b sample 1 0

count_ :: Int -> Int -> Increment -> Increment -> Int -> Int -> Int -> Int
count_ a b next_a next_b sample_max s x
  | s == sample_max = x
  | otherwise   = count_ (next_a a) (next_b b) next_a next_b sample_max (s + 1) (if cmp a b then x + 1 else x)

startValues :: String -> (Int, Int)
startValues input =
  let inputs = rowToInts input in (head inputs, last inputs)

-- export

-- examples: 65 & 8921
part1 :: String -> String
part1 input =
  let (a, b) = startValues input in
    show $ count a b nextA nextB sampleA

part2 :: String -> String
part2 input =
  let (a, b) = startValues input in
    show $ count (nextA2 a) (nextB2 b) nextA2 nextB2 sampleB
