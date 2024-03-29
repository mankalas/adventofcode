-- |
module StringHelper where

import Data.Char
import Data.List

binaryToInt :: String -> Int
binaryToInt s =
  let (_, digits) =
        mapAccumR (\acc c -> (acc + 1, (ord c - ord '0') * 2 ^ acc)) 0 s
   in sum digits
