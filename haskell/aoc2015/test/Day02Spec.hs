-- |
module Main where

import AoC (testDay)
import Test.Tasty

import Day02

tests :: [TestTree]
tests = [testDay 2015 Day02.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2015" tests)
