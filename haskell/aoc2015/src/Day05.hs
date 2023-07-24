module Day05
  ( parts
  ) where

import AoC
import MyList

-- It contains at least three vowels (aeiou only), like aei, xazegov,
-- or aeiouaeiouaeiou.
a :: String -> Bool
a s = length (filter (`elem` "aeiou") s) >= 3

-- It contains at least one letter that appears twice in a row, like
-- xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
b :: String -> Bool
b (x1:x2:xs) = x1 == x2 || b (x2 : xs)
b _ = False

-- It does not contain the strings ab, cd, pq, or xy, even if they are
-- part of one of the other requirements.
c :: String -> Bool
c =
  not .
  any (`elem` [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]) .
  consecutives

-- It contains a pair of any two letters that appears at least twice
-- in the string without overlapping, like xyxy (xy) or aabcdefgaa
-- (aa), but not like aaa (aa, but it overlaps).
f :: String -> Bool
f (x1:x2:xs) = (x1, x2) `elem` consecutives xs || f (x2 : xs)
f _ = False

-- It contains at least one letter which repeats with exactly one
-- letter between them, like xyx, abcdefeghi (efe), or even aaa.
g :: String -> Bool
g (x1:x2:x3:xs) = (x1 == x3) || g (x2 : x3 : xs)
g _ = False

nice1 :: String -> Bool
nice1 s = a s && b s && c s

nice2 :: String -> Bool
nice2 s = f s && g s

-- exports
part1 :: PartSolution
part1 = show . length . filter nice1 . lines

part2 :: PartSolution
part2 = show . length . filter nice2 . lines

parts :: DaySolutions
parts = (5, part1, part2)
