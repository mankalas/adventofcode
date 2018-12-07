-- | 

module Common where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Printf (printf)

common_tests n _module = [testCase (printf "Day %02d (example)" n) (exampleTestCase n _module),
                          testCase (printf "Day %02d (actual)" n)  (actualTestCase n _module)]

dayTestCase dir n _module = do
  input <- readFile (printf "../input/%s/Day%02d.in" dir n)
  output <- readFile (printf "../output/%s/Day%02d.out" dir n)
  let outputs = words output
      output_part1 = head outputs
      output_part2 = last outputs in do
    assertEqual "Part 1" output_part1 (_module.part1 input)
    assertEqual "Part 2" output_part2 (_module.part2 input)

exampleTestCase = do dayTestCase "example"
actualTestCase = do dayTestCase "actual"

-- main

common_main :: IO ()
common_main n _module = do
  defaultMain (testGroup (printf "AoC 2017 Day %02d" n) (common_tests n _module))
