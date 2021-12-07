-- |
module Main where

import AoC (testDay)
import Test.Tasty

import Day04

tests :: [TestTree]
tests = [testDay 2021 Day04.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2021" tests)
