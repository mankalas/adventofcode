-- | 

module CommonSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Printf (printf)

common_tests n p1 p2 = [testCase (printf "Day %02d (example)" n) (exampleTestCase n p1 p2),
                          testCase (printf "Day %02d (actual)" n) (actualTestCase n p1 p2)]

dayTestCase dir n p1 p2 = do
  input <- readFile (printf "../input/%s/Day%02d.in" dir n)
  output <- readFile (printf "../output/%s/Day%02d.out" dir n)
  let outputs = words output
      output_part1 = head outputs
      output_part2 = last outputs in do
    assertEqual "Part 1" output_part1 (p1 input)
    assertEqual "Part 2" output_part2 (p2 input)

exampleTestCase = do dayTestCase "example"
actualTestCase = do dayTestCase "actual"

-- main

common_main :: Int -> (String -> String) -> (String -> String) -> IO ()
common_main n p1 p2 = do
  defaultMain (testGroup (printf "AoC 2017 Day %02d" n) (common_tests n p1 p2))
