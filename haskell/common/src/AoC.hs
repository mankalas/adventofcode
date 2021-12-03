-- |
module AoC where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

type PartSolution = String -> String

type DaySolutions = (Int, PartSolution, PartSolution)

testDay :: Int -> DaySolutions -> TestTree
testDay year (day, part1, part2) =
  testCaseSteps ("Day " ++ d) $ \step -> do
    input <- readFile ("../../input/" ++ y ++ "/day_" ++ d)
    step "Part 1"
    output_1 <- readFile ("../../output/" ++ y ++ "/day_" ++ d ++ "_part_1")
    part1 input @?= output_1
    step "Part 2"
    output_2 <- readFile ("../../output/" ++ y ++ "/day_" ++ d ++ "_part_2")
    part2 input @?= output_2
  where
    d = printf "%02d" day
    y = show year
