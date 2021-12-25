-- |
module MyListSpec where

import Test.Tasty
import Test.Tasty.HUnit

import MyList

tests :: [TestTree]
tests = [distribute2Test, consecutivesTest]

distribute2Test :: TestTree
distribute2Test =
  testCase "distribute2" $ do
    distribute2 "abcdef" @?= ("ace", "bdf")
    distribute2 "" @?= ([], [])
    distribute2 "a" @?= ("a", [])

consecutivesTest :: TestTree
consecutivesTest =
  testCase "consecutives" $ do
    consecutives [1, 2, 3, 4, 5] @?= [(1, 2), (2, 3), (3, 4), (4, 5)]
    consecutives [1] @?= []

myListTest :: TestTree
myListTest = testGroup "MyList" tests
