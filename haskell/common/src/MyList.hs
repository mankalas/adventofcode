-- |
module MyList where

import Control.Applicative (liftA2)
import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.Tuple (swap)

-- Rotate a list by concatenating its tail with its head
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

--
filterConsecutive :: ((a, a) -> Bool) -> [a] -> [(a, a)]
filterConsecutive predicate = filter predicate . consecutives

consecutives :: [a] -> [(a, a)]
consecutives l = zip l $ init $ rotate l

sg :: Ord a => [a] -> [[a]]
sg = group . sort

allUnique :: Ord a => [a] -> Bool
allUnique = all ((==) 1 . length) . sg

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

occurrences :: Ord a => [a] -> [(Int, [a])]
occurrences = merge . map swap . count_

merge :: Eq a => [(a, b)] -> [(a, [b])]
merge [] = []
merge ((x, y):xs) = (x, y : map snd ys) : merge zs
  where
    (ys, zs) = span ((== x) . fst) xs

-- | 'count' of each element in the list, it sorts by keys (elements). Example:
--
-- > count "foo bar" == [(' ',1),('a',1),('b',1),('f',1),('o',2),('r',1)]
count :: Ord a => [a] -> [(a, Int)]
count = map lh . sg

-- | 'count_' of each elements in the list, it sorts by their number. Example:
--
-- > count_ "foo bar" == [(' ',1),('a',1),('b',1),('f',1),('r',1),('o',2)]
count_ :: Ord a => [a] -> [(a, Int)]
count_ = sortBy (compare `on` snd) . count

lh :: [a] -> (a, Int)
lh = liftA2 (,) head length

distribute2 :: [a] -> ([a], [a])
distribute2 [] = ([], [])
distribute2 [x] = ([x], [])
distribute2 [x, y] = ([x], [y])
distribute2 (x1:x2:xs) =
  let (dx, dy) = distribute2 xs
   in (x1 : dx, x2 : dy)
