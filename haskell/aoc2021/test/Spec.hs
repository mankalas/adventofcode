module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Day02

tests = [day01_1, day01_2, day02_1, day02_2]

day02_1 :: TestTree
day02_1 =
  testCase "Day 02 part 1" $ do
    Day02.part1 "" @?= ""
    checkAnswer "02" "" Day02.part1

day02_2 :: TestTree
day02_2 =
  testCase "Day 02 part 1" $ do
    Day02.part2 "" @?= ""
    checkAnswer "" "" Day02.part2

day01_1 :: TestTree
day01_1 =
  testCase "Day 01 part 1" $ do
    Day01.part1 "199\n200\n208\n210\n200\n207\n240\n269\n260\n263" @?= "7"
    checkAnswer "01" "1722" Day01.part1

day01_2 :: TestTree
day01_2 =
  testCase "Day 01 part 1" $ do
    Day01.part2 "199\n200\n208\n210\n200\n207\n240\n269\n260\n263" @?= "5"
    checkAnswer "01" "1748" Day01.part2

-- helpers
testCaseLabel day part = "Day " ++ day ++ " part " ++ show part

dayTestCase :: String -> Int -> (String -> String, String) -> TestTree
dayTestCase day part (fun, solution) =
  testCase (testCaseLabel day part) (checkAnswer day solution fun)

checkAnswer :: String -> String -> (String -> String) -> IO ()
checkAnswer d n f = do
  input <- readFile ("../../input/2021/day_" ++ d)
  assertEqual "" n (f input)

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2021" tests)
