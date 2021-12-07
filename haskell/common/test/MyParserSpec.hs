-- |
module MyParserSpec where

import Test.Tasty
import Test.Tasty.HUnit

import MyParser

tests :: [TestTree]
tests = [intsTest]

intsTest :: TestTree
intsTest =
  testCase "ints" $ do
    parseInput ints "1,2,3" @?= [1, 2, 3]
    parseInput ints "1" @?= [1]
    parseInput ints "" @?= []

myParserTest :: TestTree
myParserTest = testGroup "MyParser" tests
