-- |
module MyTuple where

toList :: (a, a) -> [a]
toList (a, b) = [a, b]

ap2 :: (a -> b) -> (a, a) -> (b, b)
ap2 f (a, b) = (f a, f b)

call2 :: (a -> b, a -> c) -> a -> (b, c)
call2 (f, g) a = (f a, g a)

map2 :: (a -> b, a -> c) -> [a] -> ([b], [c])
map2 (f, g) a = (map f a, map g a)

dot2 :: (b -> c) -> (a1 -> b, a2 -> b) -> (a1 -> c, a2 -> c)
dot2 h (f, g) = (h . f, h . g)

combine :: (a -> b -> c) -> (a, b) -> c
combine f (a, b) = f a b
