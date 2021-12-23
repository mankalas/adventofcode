-- |
module Main where

import Test.Tasty

import GeometrySpec (geometryTest)
import GridSpec (gridTest)
import MyListSpec (myListTest)
import MyParserSpec (myParserTest)

tests :: [TestTree]
tests = [gridTest, myParserTest, geometryTest, myListTest]

main :: IO ()
main = do
  defaultMain (testGroup "Haskell Common" tests)
