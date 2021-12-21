-- |
module MyParserSpec where

import Test.Tasty
import Test.Tasty.HUnit

import MyParser

tests :: [TestTree]
tests = [numbersTest, numberTest]

numbersTest :: TestTree
numbersTest =
  testCase "numbers" $ do
    parseWith (numbers ",") "1,2,314" @?= [1, 2, 314]
    parseWith (numbers "@") "1@2@314" @?= [1, 2, 314]
    parseWith (numbers "") "12314" @?= [12314]
    parseWith (numbers "") "1,2,314" @?= [1]
    parseWith (numbers "") "1234" @?= [1234]
    parseWith (numbers "") "" @?= []

numberTest :: TestTree
numberTest =
  testCase "number" $ do
    parseWith number "0" @?= 0
    parseWith number "1" @?= 1
    parseWith number "-1" @?= -1
    parseWith number "42" @?= 42
    parseWith number "-42" @?= -42

myParserTest :: TestTree
myParserTest = testGroup "MyParser" tests
