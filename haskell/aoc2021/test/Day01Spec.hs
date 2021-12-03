-- |
module Main where

import AoC (testDay)
import Test.Tasty

import Day01

tests = [testDay 2021 Day01.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2021" tests)
