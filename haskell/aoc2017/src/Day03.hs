module Day03
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Grid

type MapGrid = Map.Map Coord Int

type Coord = (Int, Int)

whichSquare :: Int -> [Int]
whichSquare n = fromJust $ find (n `elem`) squares
  where
    squares = [1] : splitPlaces (map (8 *) [1 ..]) [2 ..]

steps :: [Int] -> [(Int, Int)]
steps sq =
  let l = length sq
      n = l `div` 8
      m = n * 2
      sq_shift = last sq : init sq
      nm = [n .. m]
      mn = reverse nm
      s = take l $ cycle $ init $ mn ++ tail nm
   in zip sq_shift s

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x + a, y + b) | a <- l, b <- l, (a, b) /= (0, 0)]
  where
    l = [-1 .. 1]

compute :: Coord -> MapGrid -> Int
compute p g = sum [fromMaybe 0 $ Map.lookup c g | c <- neighbours p]

peek :: Coord -> MapGrid -> Direction -> Maybe Int
peek c g d = Map.lookup (go c d) g

nextCoord :: Coord -> MapGrid -> Coord
nextCoord p grid =
  let e = peek p grid East
      n = peek p grid North
      w = peek p grid West
      s = peek p grid South
   in case (n, s, w, e) of
        (_, Nothing, Nothing, Nothing) -> go p East
        (Nothing, Nothing, _, Nothing) -> go p North
        (Nothing, _, Nothing, Nothing) -> go p West
        (Nothing, Nothing, Nothing, _) -> go p South
        (_, Nothing, _, Nothing) -> go p East
        (Nothing, _, _, Nothing) -> go p North
        (Nothing, _, Nothing, _) -> go p West
        (_, Nothing, Nothing, _) -> go p South
        _ -> error "impossible next coord"

build :: Int -> Coord -> MapGrid -> Int
build n coord grid =
  let val = compute coord grid
      next = nextCoord coord grid
   in if val > n
        then val
        else build n next (Map.insert coord val grid)

-- exports
part1 :: String -> String
part1 input =
  show $ snd $ fromJust $ find ((== n) . fst) $ steps $ whichSquare n
  where
    n = read input

part2 :: String -> String
part2 input = show $ build n (1, 0) (Map.singleton (0, 0) 1)
  where
    n = read input
