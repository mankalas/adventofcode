module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Common
import Day01

tests = [testCase "Day 01 (example)" exampleTestCase,
         testCase "Day 01 (actual)"  actualTestCase]

dayTestCase dir = do
  input <- readFile ("../input/" ++ dir ++ "/Day01.in")
  output <- readFile ("../output/" ++ dir ++ "/Day01.out")
  let outputs = words output
      output_part1 = head outputs
      output_part2 = last outputs in do
    assertEqual "Part 1" output_part1 (Day01.part1 input)
    assertEqual "Part 2" output_part2 (Day01.part2 input)

exampleTestCase = do dayTestCase "example"
actualTestCase = do dayTestCase "actual"

-- main

main :: IO ()
main = do
  defaultMain (testGroup "AoC 2017 Day 01" tests)
