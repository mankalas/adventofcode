module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as V

import AoC (testDay)
import Common
import Day01
import Day02

tests = [testDay 2017 Day01.parts]

-- main
main :: IO ()
main = do
  defaultMain (testGroup "AoE 2017" tests)
