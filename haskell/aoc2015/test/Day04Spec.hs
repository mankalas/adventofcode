-- |
module Main where

import AoC (testDay)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Day04 (parts)

tests :: [TestTree]
tests = [testDay 2015 Day04.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2015" tests)
