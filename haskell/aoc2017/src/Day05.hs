module Day05
  ( part1
  , part2
  ) where

import qualified Data.Vector as V

import MyVector

type OffsetSucc = Int -> Int

pom :: OffsetSucc
pom n
  | n >= 3 = -1
  | otherwise = 1

move :: Int -> Int -> OffsetSucc -> VInt -> Int
move pos n f v =
  let offset = v V.! pos
      next_pos = pos + offset
   in if next_pos `elem` [0 .. V.length v]
        then n + 1
        else let incr = f offset
              in move next_pos (n + 1) f (increment pos incr v)

countSteps :: OffsetSucc -> String -> String
countSteps o_succ =
  show . move 0 0 o_succ . V.fromList . Prelude.map read . lines

-- exports
part1 :: String -> String
part1 = countSteps $ const 1

part2 :: String -> String
part2 = countSteps pom
