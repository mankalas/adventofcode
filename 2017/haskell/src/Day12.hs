module Day12 (part1, part2) where

import Debug.Trace

import Parser

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

import Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

type Pipe = (Int, [Int])
type ComMap = M.Map Int [Int]
type SetI = S.Set (Int)

-- parser

parsePipes :: String -> [Pipe]
parsePipes = parseInput pipes

pipes :: GenParser Char () [Pipe]
pipes = do
  p <- many pipe
  eof
  return p

pipe :: GenParser Char () Pipe
pipe = do
  guy <- int
  string " <-> "
  dests <- guys
  endOfLine
  return (guy, dests)

guys :: GenParser Char () [Int]
guys = int `sepBy` (string ", ")

-- code

buildMap :: [Pipe] -> ComMap
buildMap pipes = Prelude.foldl buildMap_ M.empty pipes
  where buildMap_ m (src, dst) = M.insert src dst m

buildSet :: ComMap -> SetI
buildSet m = buildSet_ m S.empty 0

buildSet_ :: ComMap -> SetI -> Int -> SetI
buildSet_ m s i =
  case M.lookup i m of
    Nothing      -> s
    Just targets -> S.unions (S.fromList targets:(map (buildSet_ new_map s) targets))
      where new_map = M.delete i m

buildGroups :: ComMap -> [SetI]
buildGroups m = buildGroups_ m []

buildGroups_ :: ComMap -> [SetI] -> [SetI]
buildGroups_ m s =
  case L.find not_in_any_set (M.keys m) of
    Nothing -> s
    Just v  -> buildGroups_ m (buildSet_ m S.empty v:s)
  where not_in_any_set k = all (not . S.member k) s

-- export

part1 :: String -> String
part1 input = show $ S.size $ buildSet $ buildMap $ parsePipes input

part2 :: String -> String
part2 input = show $ length $ buildGroups $ buildMap $ parsePipes input
