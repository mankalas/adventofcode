-- |
module Grid where

import Data.List as L
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

type Grid a = [[a]]

row :: Grid a -> Int -> [a]
row g n = g !! n

column :: Grid a -> Int -> [a]
column g n = (transpose g) !! n

filter :: (a -> Bool) -> Grid a -> [a]
filter f = L.filter f . concat

rowsAndCols :: Grid a -> [[a]]
rowsAndCols g = g ++ transpose g

anyRowOrCol :: (a -> Bool) -> Grid a -> Bool
anyRowOrCol f = any (all f) . rowsAndCols

findRowOrCol :: (a -> Bool) -> Grid a -> Maybe [a]
findRowOrCol f = find (all f) . rowsAndCols

up :: Coord -> Coord
up (x, y) = (x, y + 1)

down :: Coord -> Coord
down (x, y) = (x, y - 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

north :: Coord -> M.Map Coord a -> Maybe a
north p = M.lookup (up p)

south :: Coord -> M.Map Coord a -> Maybe a
south p = M.lookup (down p)

west :: Coord -> M.Map Coord a -> Maybe a
west p = M.lookup (left p)

east :: Coord -> M.Map Coord a -> Maybe a
east p = M.lookup (right p)

cardinalPoints :: Coord -> M.Map Coord a -> [Maybe a]
cardinalPoints c grid =
  let neighbors = map (\f -> f c) [up, down, left, right]
   in map (\n -> M.lookup n grid) neighbors
