module Day02
  ( parts
  ) where

import AoC
import Geometry
import MyParser

type Box = RectangularPrism

boxesParser :: String -> [Box]
boxesParser =
  parseLinesWith $ do
    b <- numbers "x"
    pure (RectangularPrism (b !! 0) (b !! 1) (b !! 2))

wrappingArea :: Box -> Int
wrappingArea b = (prismArea b) + (prismSmallestSideArea b)

ribbonLength :: Box -> Int
ribbonLength b = (prismVolume b) + (prismSmallestSidePerimeter b)

-- exports
part1 :: PartSolution
part1 s =
  let boxes = boxesParser s
   in show $ foldr (\b acc -> acc + wrappingArea b) 0 boxes

part2 :: PartSolution
part2 s =
  let boxes = boxesParser s
   in show $ foldr (\b acc -> acc + ribbonLength b) 0 boxes

parts :: DaySolutions
parts = (2, part1, part2)
