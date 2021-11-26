-- |
module Grid where

import qualified Data.Map.Strict as M

type Coord = (Int, Int)

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
