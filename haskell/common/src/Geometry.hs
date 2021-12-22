-- |
module Geometry where

data RectangularPrism =
  RectangularPrism Int Int Int

prismArea :: RectangularPrism -> Int
prismArea (RectangularPrism h w l) = 2 * h * w + 2 * h * l + 2 * w * l

prismSmallestSideArea :: RectangularPrism -> Int
prismSmallestSideArea (RectangularPrism h w l) = minimum [h * w, h * l, w * l]
