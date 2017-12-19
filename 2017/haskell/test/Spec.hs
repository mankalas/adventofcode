module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Day01

main :: IO ()
main = do
  defaultMain (testGroup "My tests" [year2017day01part1])

year2017day01part1 :: TestTree
year2017day01part1 = testCase "2017 Day 01 part 1"
  (assertEqual "" (sumCaptcha "3") 3)
