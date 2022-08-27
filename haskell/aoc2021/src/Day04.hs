module Day04 where

import Data.List as L (find)
import Text.Parsec as P (count, many, sepBy)

import AoC (DaySolutions, PartSolution)
import Grid as G (Grid, anyRowOrCol, filter)
import MyParser (number, parseWith, symbol)

type Board = G.Grid Int

-- parse
bingoParser :: String -> ([Int], [Board])
bingoParser =
  parseWith $ do
    n <- number `sepBy` symbol ","
    b <- many board
    pure (n, b)
  where
    board = P.count 5 $ P.count 5 number

-- game
sumBoard :: (Int -> Bool) -> Board -> Int
sumBoard cellFilter = sum . G.filter cellFilter

draw :: Int -> [Board] -> [Board]
draw _ bs = bs

hasWon :: [Int] -> Board -> Bool
hasWon draws = anyRowOrCol (`elem` draws)

play :: [Int] -> [Board] -> ([Int], Board)
play [] _ = ([], [])
play draws@(n:ns) bs =
  case find (hasWon draws) bs of
    Nothing -> play ns $ draw n bs
    Just winner -> (draws, winner)

result :: [Int] -> [Board] -> Int
result ns bs =
  let (draws, winner) = play ns bs
   in sumBoard (`elem` draws) winner * sumBoard (`notElem` draws) winner

-- exports
part1 :: PartSolution
part1 input =
  let (n, b) = bingoParser input
   in show $ result n b

part2 :: PartSolution
part2 = id

parts :: DaySolutions
parts = (4, part1, part2)
