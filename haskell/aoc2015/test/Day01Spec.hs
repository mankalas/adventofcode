-- |
module Main where

import AoC (testDay)
import Test.Tasty

import Day01

tests :: [TestTree]
tests = [testDay 2015 Day01.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2015" tests)
