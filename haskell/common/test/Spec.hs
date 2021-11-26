-- |
module Main where

import Test.Tasty

import GridSpec (gridTest)

tests :: [TestTree]
tests = [gridTest]

main :: IO ()
main = do
  defaultMain (testGroup "Haskell Common" tests)
