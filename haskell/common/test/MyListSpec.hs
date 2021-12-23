-- |
module MyListSpec where

import Test.Tasty
import Test.Tasty.HUnit

import MyList

tests :: [TestTree]
tests = [distribute2Test]

distribute2Test :: TestTree
distribute2Test =
  testCase "distribute2" $ do
    distribute2 "abcdef" @?= ("ace", "bdf")
    distribute2 "" @?= ([], [])
    distribute2 "a" @?= ("a", [])

myListTest :: TestTree
myListTest = testGroup "MyList" tests
