-- |
module GridSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Grid
  ( Direction(East, North, South, West)
  , column
  , filter
  , findRowOrCol
  , go
  , range
  , range'
  , row
  )

tests :: [TestTree]
tests = [goTest, rowTest, columnTest, filterTest, findRowOrColTest, rangeTest]

goTest :: TestTree
goTest =
  testCase "go" $ do
    go (0, 0) North @?= (0, 1)
    go (1, 1) North @?= (1, 2)
    go (-5, -5) North @?= (-5, -4)
    go (0, 0) South @?= (0, -1)
    go (1, 1) South @?= (1, 0)
    go (-5, -5) South @?= (-5, -6)
    go (0, 0) West @?= (-1, 0)
    go (1, 1) West @?= (0, 1)
    go (-5, -5) West @?= (-6, -5)
    go (0, 0) East @?= (1, 0)
    go (1, 1) East @?= (2, 1)
    go (-5, -5) East @?= (-4, -5)

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
    Grid.filter odd [[1, 2, 3], [4, 5, 6], [7, 8, 9]] @?= [1, 3, 5, 7, 9]

findRowOrColTest :: TestTree
findRowOrColTest =
  testCase "find row or col" $ do
    findRowOrCol odd [[1, 2], [3, 4]] @?= Just [1, 3]
    findRowOrCol odd [[1, 3], [2, 4]] @?= Just [1, 3]
    findRowOrCol odd [[2, 4], [6, 8]] @?= Nothing
    findRowOrCol odd [[2, 1], [3, 4]] @?= Nothing

rangeTest :: TestTree
rangeTest =
  testCase "range / range'" $ do
    range (0, 0) (2, 2) @?=
      [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
    range' (0, 0) (2, 2) @?=
      [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

gridTest :: TestTree
gridTest = testGroup "Grid" tests
