module Day03 (part1, part2) where

import Common (Coord, north, east, west, south, left, right, up, down)

import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

type Grid = Map.Map Coord Int

whichSquare :: Int -> [Int]
whichSquare n = fromJust $ find (n `elem`) squares
  where squares = [[1]] ++ splitPlaces (map (8*) [1..]) [2..]

steps :: [Int] -> [(Int, Int)]
steps sq =
  let l = length sq
      n = l `div` 8
      m = n * 2
      sq_shift = last sq:init sq
      nm = [n..m]
      mn = reverse nm
      steps = take l $ cycle $ init $ mn ++ tail nm in
    zip sq_shift steps

neighbours :: Coord -> [Coord]
neighbours (x, y) = [ (x + a, y + b) | a <- l, b <- l, (a, b) /= (0, 0) ]
  where l = [-1..1]

compute :: Coord -> Grid -> Int
compute p g = sum [ fromMaybe 0 $ Map.lookup c g | c <- neighbours p ]

nextCoord :: Coord -> Grid -> Coord
nextCoord p grid =
  let e = east p grid
      n = north p grid
      w = west p grid
      s = south p grid in
    case (n, s, w, e) of
      (_,       Nothing, Nothing, Nothing) -> right p
      (Nothing, Nothing, _,       Nothing) -> up p
      (Nothing, _,       Nothing, Nothing) -> left p
      (Nothing, Nothing, Nothing, _      ) -> down p
      (_,       Nothing, _,       Nothing) -> right p
      (Nothing, _,       _,       Nothing) -> up p
      (Nothing, _,       Nothing, _      ) -> left p
      (_,       Nothing, Nothing, _      ) -> down p

build :: Int -> Coord -> Grid -> Int
build n coord grid =
  let val = compute coord grid
      next = nextCoord coord grid in
    if val > n
    then val
    else build n next (Map.insert coord val grid)

-- exports

part1 :: String -> String
part1 input = show $ snd $ fromJust $ find ((== n) . fst) $ steps $ whichSquare n
  where n = read input

part2 :: String -> String
part2 input = show $ build n (1, 0) (Map.singleton (0, 0) 1)
  where n = read input
