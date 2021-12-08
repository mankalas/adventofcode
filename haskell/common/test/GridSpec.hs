-- |
module GridSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Grid as G

tests :: [TestTree]
tests =
  [ upTest
  , downTest
  , leftTest
  , rightTest
  , rowTest
  , columnTest
  , filterTest
  , findRowOrColTest
  ]

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

rowTest :: TestTree
rowTest =
  testCase "row" $ do
    row [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 0 @?= [1, 2, 3]
    row [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 1 @?= [4, 5, 6]

columnTest :: TestTree
columnTest =
  testCase "column" $ do
    column [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 0 @?= [1, 4, 7]
    column [[1, 2, 3], [4, 5, 6], [7, 8, 9]] 1 @?= [2, 5, 8]

filterTest :: TestTree
filterTest =
  testCase "filter" $ do
    G.filter odd [[1, 2, 3], [4, 5, 6], [7, 8, 9]] @?= [1, 3, 5, 7, 9]

findRowOrColTest :: TestTree
findRowOrColTest =
  testCase "find row or col" $ do
    findRowOrCol odd [[1, 2], [3, 4]] @?= Just [1, 3]
    findRowOrCol odd [[1, 3], [2, 4]] @?= Just [1, 3]
    findRowOrCol odd [[2, 4], [6, 8]] @?= Nothing
    findRowOrCol odd [[2, 1], [3, 4]] @?= Nothing

gridTest :: TestTree
gridTest = testGroup "Grid" tests
