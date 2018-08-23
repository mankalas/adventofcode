module Day14 (part1, part2) where

import Common
import Day10 (knotHash)

import Debug.Trace

import Numeric (showIntAtBase, readHex)
import Data.Char (intToDigit, digitToInt)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Set as S

type Grid = V.Vector (VInt)
type RegionGrid = M.Map Coord Int
type SCoord = S.Set (Coord)

paddedBin :: Char -> String
paddedBin c =
  let b = showIntAtBase 2 intToDigit (fst $ head $ readHex [c]) "" in
    (take (4 - length b) $ cycle "0") ++ b

suffixInput :: String -> Int -> String
suffixInput input = (++) input . (++) "-" . show

createLine :: String -> VInt
createLine = V.fromList . map digitToInt . intercalate "" . map paddedBin . knotHash

grid :: String -> Grid
grid input = V.generate 128 $ createLine . suffixInput input

lookup :: Coord -> Grid -> Int
lookup (-1, _) _ = 0
lookup (_, -1) _ = 0
lookup (128, _) _ = 0
lookup (_, 128) _ = 0
lookup (x, y) g = (g V.! y) V.! x

nextCell :: Coord -> Coord
nextCell (127, y) = (0, y + 1)
nextCell (x, y) = (x + 1, y)

countRegions :: Grid -> Int
countRegions grid = countRegions_ (0, 0) 0 S.empty grid

skip :: Coord -> Grid -> SCoord -> Bool
skip c g s = Day14.lookup c g == 0 || S.member c s

explore :: Grid -> SCoord -> Coord -> SCoord
explore grid s c =
  if skip c grid s then
    s
  else
    foldl (explore grid) (S.insert c s) [up(c), down(c), left(c), right(c)]

countRegions_ :: Coord -> Int -> SCoord -> Grid -> Int
countRegions_ (0, 128) num_regs _ _ = num_regs
countRegions_ c@(x, y) num_regs s grid =
  if skip c grid s then
    countRegions_ nc num_regs s grid
  else
    countRegions_ nc (num_regs + 1) (explore grid s c) grid
  where nc = nextCell c

-- export

part1 :: String -> String
part1 = show . sum . V.map sum . grid

part2 :: String -> String
part2 = show . countRegions . grid
