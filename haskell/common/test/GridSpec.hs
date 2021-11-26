-- |
module GridSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Grid (down, left, right, up)

tests :: [TestTree]
tests = [upTest, downTest, leftTest, rightTest]

upTest :: TestTree
upTest =
  testCase "up" $ do
    up (0, 0) @?= (0, 1)
    up (1, 1) @?= (1, 2)
    up (-5, -5) @?= (-5, -4)

downTest :: TestTree
downTest =
  testCase "down" $ do
    down (0, 0) @?= (0, -1)
    down (1, 1) @?= (1, 0)
    down (-5, -5) @?= (-5, -6)

leftTest :: TestTree
leftTest =
  testCase "left" $ do
    left (0, 0) @?= (-1, 0)
    left (1, 1) @?= (0, 1)
    left (-5, -5) @?= (-6, -5)

rightTest :: TestTree
rightTest =
  testCase "right" $ do
    right (0, 0) @?= (1, 0)
    right (1, 1) @?= (2, 1)
    right (-5, -5) @?= (-4, -5)

gridTest :: TestTree
gridTest = testGroup "Grid" tests
