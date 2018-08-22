module Day14 (part1, part2) where

import Common
import Day10 (knotHash)

import Debug.Trace

import Numeric (showIntAtBase, readHex)
import Data.Char (intToDigit, digitToInt)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Grid = V.Vector (VInt)
type RegionGrid = M.Map Coord Int

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
lookup (x, y) g = (g V.! y) V.! x

nextCell :: Coord -> Coord
nextCell (127, y) = (0, y + 1)
nextCell (x, y) = (x + 1, y)

countRegions :: Grid -> Int
countRegions grid = countRegions_ (0, 0) 0 grid

countRegions_ :: Coord -> Int -> Grid -> Int
countRegions_ (0, 128) num_regs _ = num_regs
countRegions_ c@(x, y) num_regs grid =
  let w  = Day14.lookup (x - 1, y) grid
      n  = Day14.lookup (x, y - 1) grid
      nw = Day14.lookup (x - 1, y - 1) grid
      v  = Day14.lookup c grid
      nc = nextCell c
  in
    case (w, n, v, nw) of
      (1, 1, 1, 0) -> countRegions_ nc (num_regs - 1) grid
      (0, 0, 1, _) -> countRegions_ nc (num_regs + 1) grid
      _            -> countRegions_ nc num_regs grid

-- export

part1 :: String -> String
part1 = show . sum . V.map sum . grid

part2 :: String -> String
part2 = show . countRegions . grid
