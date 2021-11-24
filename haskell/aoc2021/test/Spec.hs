module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Day01

tests = [day01_1, day01_2]

day01_1 :: TestTree
day01_1 = dayTestCase "01" 1 (Day01.part1, "lorem ipsum")

day01_2 :: TestTree
day01_2 = dayTestCase "01" 2 (Day01.part2, "lorem ipsum")

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
