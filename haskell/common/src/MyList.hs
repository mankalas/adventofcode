-- |
module MyList where

-- Rotate a list by concatenating its tail with its head
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

--
filterConsecutive :: ((a, a) -> Bool) -> [a] -> [(a, a)]
filterConsecutive predicate l1 =
  let l2 = rotate l1
      l = zip l1 l2
   in filter predicate l
