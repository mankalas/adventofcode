-- |
module StringHelper where

import Data.Char (ord)
import Data.List (mapAccumR)

binaryToInt :: String -> Int
binaryToInt s =
  let (_, digits) =
        mapAccumR (\acc c -> (acc + 1, (ord c - ord '0') * 2 ^ acc)) 0 s
   in sum digits
