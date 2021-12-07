-- |
module Main where

import Test.Tasty

import GridSpec (gridTest)
import MyParserSpec (myParserTest)

tests :: [TestTree]
tests = [gridTest, myParserTest]

main :: IO ()
main = do
  defaultMain (testGroup "Haskell Common" tests)
