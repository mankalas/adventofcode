-- |
module MyParserSpec where

import Test.Tasty
import Test.Tasty.HUnit

import MyParser

tests :: [TestTree]
tests = [intsTest, intTest]

intsTest :: TestTree
intsTest =
  testCase "ints" $ do
    parseWith ints "1,2,314" @?= [1, 2, 314]
    parseWith ints "1" @?= [1]
    parseWith ints "" @?= []

numberTest :: TestTree
numberTest =
  testCase "int" $ do
    parseWith int "0" @?= 0
    parseWith int "1" @?= 1
    parseWith int "-1" @?= -1
    parseWith int "42" @?= 42
    parseWith int "-42" @?= -42

myParserTest :: TestTree
myParserTest = testGroup "MyParser" tests
