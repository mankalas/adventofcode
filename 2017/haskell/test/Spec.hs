module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

tests = [
  day01_1, day01_2,
  day02_1, day02_2,
  day03_1, day03_2,
  day04_1, day04_2,
  day05_1, day05_2,
  day06_1, day06_2
  ]

-- tests

day01_1 :: TestTree
day01_1 = dayTestCase "01" 1 (Day01.part1, "1047")

day01_2 :: TestTree
day01_2 = dayTestCase "01" 2 (Day01.part2, "982")

day02_1 :: TestTree
day02_1 = dayTestCase "02" 1 (Day02.part1, "47136")

day02_2 :: TestTree
day02_2 = dayTestCase "02" 2 (Day02.part2, "250")

day03_1 :: TestTree
day03_1 = dayTestCase "03" 1 (Day03.part1, "419")

day03_2 :: TestTree
day03_2 = dayTestCase "03" 2 (Day03.part2, "")

day04_1 :: TestTree
day04_1 = dayTestCase "04" 1 (Day04.part1, "325")

day04_2 :: TestTree
day04_2 = dayTestCase "04" 2 (Day04.part2, "119")

day05_1 :: TestTree
day05_1 = dayTestCase "05" 1 (Day05.part1, "375042")

day05_2 :: TestTree
day05_2 = dayTestCase "05" 2 (Day05.part2, "28707598")

day06_1 :: TestTree
day06_1 = dayTestCase "06" 1 (Day06.part1, "4074")

day06_2 :: TestTree
day06_2 = dayTestCase "06" 2 (Day06.part2, "2793")

-- helpers

testCaseLabel day part = "Day " ++ day ++ " part " ++ show part

dayTestCase :: String -> Int -> (String -> String, String) -> TestTree
dayTestCase day part (fun, solution) =
  testCase (testCaseLabel day part) (checkAnswer day solution fun)

checkAnswer :: String -> String -> (String -> String) -> IO ()
checkAnswer d n f = do
  input <- readFile ("test/Day" ++ d ++ ".input")
  assertEqual "" n (f input)

-- main

main :: IO ()
main = do
  defaultMain (testGroup "AoE 2017" tests)
