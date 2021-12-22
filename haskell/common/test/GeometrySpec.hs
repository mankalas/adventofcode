-- |
module GeometrySpec where

import Test.Tasty
import Test.Tasty.HUnit

import Geometry

tests :: [TestTree]
tests = [rectangularPrismTest]

rectangularPrismTest :: TestTree
rectangularPrismTest =
  testCase "Rectangular Prism" $ do
    prismArea (RectangularPrism 2 3 4) @?= 52
    prismSmallestSideArea (RectangularPrism 2 3 4) @?= 6
    prismSmallestSidePerimeter (RectangularPrism 2 3 4) @?= 10
    prismVolume (RectangularPrism 2 3 4) @?= 24
    prismArea (RectangularPrism 1 1 10) @?= 42
    prismSmallestSideArea (RectangularPrism 1 1 10) @?= 1
    prismSmallestSidePerimeter (RectangularPrism 1 1 10) @?= 4
    prismVolume (RectangularPrism 1 1 10) @?= 10

geometryTest :: TestTree
geometryTest = testGroup "Geometry" tests
