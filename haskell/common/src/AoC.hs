-- |
module AoC where

import Test.Tasty
import Test.Tasty.HUnit

type Solution = String -> String

testDay i part1 part2 =
  testCaseSteps "Day" $ \step -> do
    step "Part 1"
--    part1 input @?= output_part1
    step "Part 2"
--    part2 input @?= output_part2
