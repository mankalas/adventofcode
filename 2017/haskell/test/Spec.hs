module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as V

import Common
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13

tests = [
  -- circular_test,
  -- day01_1, day01_2,
  -- day02_1, day02_2,
  -- day03_1, day03_2,
  -- day04_1, day04_2,
  -- day05_1, day05_2,
  -- day06_1, day06_2,
  -- day07_1, day07_2,
  -- day08_1, day08_2,
  -- day09_1, day09_2,
  -- day10_1, day10_2,
  -- day11_1, day11_2,
  -- day12_1, day12_2,
  day13_1, day13_2
  ]

-- tests

circular_test :: TestTree
circular_test = testCase "Circular list" $ do
  assertEqual "Classic " "[0,2,1]" $ show $ Common.circularReverse (V.fromList [0,1,2]) 1 2
  assertEqual "Circular" "[2,1,0]" $ show $ Common.circularReverse (V.fromList [0,1,2]) 2 2
  assertEqual "Circular" "[4,3,0,1,2]" $ show $ Common.circularReverse (V.fromList [2,1,0,3,4]) 3 4
  assertEqual "Circular" "[4,3,0,1,2]" $ show $ Common.circularReverse (V.fromList [4,3,0,1,2]) 4 1
  assertEqual "Circular" "[3,4,2,1,0]" $ show $ Common.circularReverse (V.fromList [4,3,0,1,2]) 1 5

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
day03_2 = dayTestCase "03" 2 (Day03.part2, "295229")

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

day07_1 :: TestTree
day07_1 = dayTestCase "07" 1 (Day07.part1, "xegshds")

day07_2 :: TestTree
day07_2 = dayTestCase "07" 2 (Day07.part2, "299")

day08_1 :: TestTree
day08_1 = dayTestCase "08" 1 (Day08.part1, "3880")

day08_2 :: TestTree
day08_2 = dayTestCase "08" 2 (Day08.part2, "5035")

day09_1 :: TestTree
day09_1 = dayTestCase "09" 1 (Day09.part1, "9662")

day09_2 :: TestTree
day09_2 = dayTestCase "09" 2 (Day09.part2, "4903")

day10_1 :: TestTree
day10_1 = dayTestCase "10" 1 (Day10.part1, "54675")

day10_2 :: TestTree
day10_2 = dayTestCase "10" 2 (Day10.part2, "a7af2706aa9a09cf5d848c1e6605dd2a")

day11_1 :: TestTree
day11_1 = dayTestCase "11" 1 (Day11.part1, "812")

day11_2 :: TestTree
day11_2 = dayTestCase "11" 2 (Day11.part2, "1603")

day12_1 :: TestTree
day12_1 = dayTestCase "12" 1 (Day12.part1, "145")

day12_2 :: TestTree
day12_2 = dayTestCase "12" 2 (Day12.part2, "207")

day13_1 :: TestTree
day13_1 = dayTestCase "13" 1 (Day13.part1, "2264")

day13_2 :: TestTree
day13_2 = dayTestCase "13" 2 (Day13.part2, "3875838")

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
