module Day03 where

import Debug.Trace

import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)
type Grid = Map.Map Coord Int

whichSquare :: Int -> [Int]
whichSquare n = fromJust $ find (n `elem`) squares
  where squares = [[1]] ++ splitPlaces (map (8*) [1..]) [2..]

steps :: [Int] -> [(Int, Int)]
steps sq =
  let l = length sq
      n = l `div` 8
      m = n * 2
      sq2 = last sq : init sq
      nm = [n..m]
      mn = reverse nm
      steps = take l $ cycle $ init $ mn ++ tail nm in
    zip sq2 steps

north (x,y) = (x, y + 1)
south (x,y) = (x, y - 1)
east  (x,y) = (x + 1, y)
west  (x,y) = (x - 1, y)

neighbours :: Coord -> [Coord]
neighbours (x,y) = [ (x + a, y + b) | a <- l, b <- l, (a, b) /= (0,0) ]
  where l = [-1..1]

compute :: Coord -> Grid -> Int
compute p g = sum [ fromMaybe 0 $ Map.lookup c g | c <- neighbours p ]

next :: Coord -> Grid -> Coord
next x@(0, 0) _ = east x
next x@(1, 0) _ = north x
next x@(1, 1) _ = west x
next x@(-1, 1) _ = south x
next x@(-1, -1) _ = east x
next p grid =
  let e = Map.lookup (east p) grid
      n = Map.lookup (north p) grid
      w = Map.lookup (west p) grid
      s = Map.lookup (south p) grid in
    if e == Nothing && s == Nothing
    then if n == Nothing then north p else east p
    else
      if n == Nothing && e == Nothing
      then if w == Nothing then west p else north p
      else
        if w == Nothing && n == Nothing
        then if s == Nothing then south p else west p
        else south p

build :: Int -> Coord -> Grid -> Int
build n coord grid =
  let val = compute coord grid in
    if val > n
    then val
    else
      let m = next coord grid in
        build (n+1) m (Map.insert coord val grid)

-- exports

part1 :: String -> String
part1 input =
  let tuples = steps $ whichSquare n
      tuple = fromJust $ find ((== n) . fst) tuples in
    show $ snd tuple
  where n = read input

part2 :: String -> String
part2 input = show $ build (read input) (1, 0) (Map.singleton (0, 0) 1)
