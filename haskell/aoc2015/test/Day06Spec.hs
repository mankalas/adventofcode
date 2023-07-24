-- |
module Main where

import AoC (testDay)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Day06 (parts)

tests :: [TestTree]
tests = [testDay 2015 Day06.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2015" tests)
