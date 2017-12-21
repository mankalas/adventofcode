module Day03 (part1, part2) where

import Common

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

neighbours :: Coord -> [Coord]
neighbours (x,y) = [ (x + a, y + b) | a <- l, b <- l, (a, b) /= (0,0) ]
  where l = [-1..1]

compute :: Coord -> Grid -> Int
compute p g = sum [ fromMaybe 0 $ Map.lookup c g | c <- neighbours p ]

nextCoord :: Coord -> Grid -> Coord
nextCoord p grid =
  let e = Map.lookup (east p) grid
      n = Map.lookup (north p) grid
      w = Map.lookup (west p) grid
      s = Map.lookup (south p) grid in
    case (n, s, w, e) of
      (_,       Nothing, Nothing, Nothing) -> east p
      (Nothing, Nothing, _,       Nothing) -> north p
      (Nothing, _,       Nothing, Nothing) -> west p
      (Nothing, Nothing, Nothing, _      ) -> south p
      (_,       Nothing, _,       Nothing) -> east p
      (Nothing, _,       _,       Nothing) -> north p
      (Nothing, _,       Nothing, _      ) -> west p
      (_,       Nothing, Nothing, _      ) -> south p
      where
         north (x, y) = (x, y + 1)
         south (x, y) = (x, y - 1)
         east  (x, y) = (x + 1, y)
         west  (x, y) = (x - 1, y)

build :: Int -> Coord -> Grid -> Int
build n coord grid =
  let val = compute coord grid
      next = nextCoord coord grid in
    if val > n then val else build n next (Map.insert coord val grid)

-- exports

part1 :: String -> String
part1 input = show $ snd $ fromJust $ find ((== n) . fst) $ steps $ whichSquare n
  where n = read input

part2 :: String -> String
part2 input = show $ build n (1, 0) (Map.singleton (0, 0) 1)
  where n = read input
