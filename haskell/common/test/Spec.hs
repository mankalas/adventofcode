-- |
module Main where

import Test.Tasty

import GeometrySpec (geometryTest)
import GridSpec (gridTest)
import MyParserSpec (myParserTest)

tests :: [TestTree]
tests = [gridTest, myParserTest, geometryTest]

main :: IO ()
main = do
  defaultMain (testGroup "Haskell Common" tests)
