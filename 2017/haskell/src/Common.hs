module Common where

import Data.Vector as V

rowToInts :: String -> [Int]
rowToInts row = Prelude.map read $ words row

increment :: Int -> Int -> V.Vector (Int) -> V.Vector (Int)
increment pos incr v = v // [(pos, v ! pos + incr)]
