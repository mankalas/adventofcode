module Day06
  ( parts
  ) where

import AoC (DaySolutions, PartSolution)
import Data.Array
import Data.List
import Debug.Trace
import Grid hiding (range)
import MyList
import MyParser
import Text.Parsec

data Action
  = Off
  | On
  | Toggle
  deriving (Show)

data Instruction =
  RecordType
    { action :: Action
    , from :: Coord
    , to :: Coord
    }

instructions :: String -> [Instruction]
instructions = parseLinesWith $ do instruction
  where
    instruction = do
      act <- try (turnOn <|> toggle <|> turnOff)
      a <- number
      _ <- string ","
      b <- number
      _ <- string "through "
      x <- number
      _ <- string ","
      y <- number
      return $
        trace (show act) RecordType {action = act, from = (a, b), to = (x, y)}
    toggle = Toggle <$ string "toggle "
    turnOn = On <$ string "turn on "
    turnOff = Off <$ string "turn off "

aptply' :: Array Coord Bool -> Instruction -> Array Coord Bool
aptply' g i =
  trace "Pouet" g //
  map
    (\c ->
       ( c
       , case action i of
           Toggle -> not (g ! c)
           On -> True
           Off -> False))
    (range (from i, to i))

applyAll :: [Instruction] -> Array Coord Bool
applyAll instr =
  trace
    (show (length instr))
    foldl'
    aptply'
    (array
       ((0, 0), (999, 999))
       [((i, j), False) | i <- [0 .. 999], j <- [0 .. 999]])
    instr

-- exports
part1 :: PartSolution
part1 input = show $ countElem True $ elems $ applyAll $ instructions input

part2 :: PartSolution
part2 s = ""

parts :: DaySolutions
parts = (6, part1, part2)
