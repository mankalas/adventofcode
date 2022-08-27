-- |
module Grid
  ( Direction(North, South, West, East)
  , Grid
  , Coord
  -- , row
  -- , column
  -- , Grid.filter
  -- , rowsAndCols
  -- , anyRowOrCol
  -- , findRowOrCol
  , go
  , up
  , down
  , left
  , right
  , range
  , range'
  ) where

import Data.Array hiding (range)
import Data.List as L (filter, find, transpose)
import Data.Vector as V

type Coord = (Int, Int)

type Grid a = Array Coord a

data Direction
  = North
  | South
  | West
  | East
  deriving (Show, Eq)

-- row :: Grid a -> Int -> V.Vector a
-- row g n = g !! n
-- column :: Grid a -> Int -> V.Vector a
-- column g n = V.transpose g !! n
-- filter :: (a -> Bool) -> Grid a -> V.Vector a
-- filter f = L.filter f . V.concat
-- rowsAndCols :: Grid a -> [V.Vector a]
-- rowsAndCols g = g V.++ transpose g
-- anyRowOrCol :: (a -> Bool) -> Grid a -> Bool
-- anyRowOrCol f = V.any (V.all f) . rowsAndCols
-- findRowOrCol :: (a -> Bool) -> Grid a -> Maybe (V.Vector a)
-- findRowOrCol f = V.find (V.all f) . rowsAndCols
go :: Coord -> Direction -> Coord
go here there =
  case there of
    North -> up here
    South -> down here
    West -> left here
    East -> right here

up :: Coord -> Coord
up (x, y) = (x, y + 1)

down :: Coord -> Coord
down (x, y) = (x, y - 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

range :: Coord -> Coord -> [Coord]
range (a, b) (x, y) = [(u, v) | u <- [a .. x], v <- [b .. y]]

range' :: Coord -> Coord -> [Coord]
range' (a, b) (x, y) = (,) <$> [a .. x] <*> [b .. y]
