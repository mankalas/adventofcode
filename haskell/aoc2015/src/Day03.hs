module Day03
  ( parts
  ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (mapAccumL)
import Text.Parsec ((<|>), char, many)

import AoC (DaySolutions, PartSolution)
import Grid (Coord, Direction(..), go)
import MyList (distribute2)
import MyParser (parseWith)
import MyTuple (ap2, combine)

parseDirections :: String -> [Direction]
parseDirections =
  parseWith $ do
    many
      ((char '^' >> return North) <|> (char 'v' >> return South) <|>
       (char '<' >> return West) <|>
       (char '>' >> return East))

deliver :: String -> [Coord]
deliver s =
  let (end, houses) =
        mapAccumL (\pos dir -> (go pos dir, pos)) (0, 0) $ parseDirections s
   in houses ++ [end]

-- exports
part1 :: PartSolution
part1 = show . length . nubOrd . deliver

part2 :: PartSolution
part2 = show . length . nubOrd . combine (++) . ap2 deliver . distribute2

parts :: DaySolutions
parts = (3, part1, part2)
