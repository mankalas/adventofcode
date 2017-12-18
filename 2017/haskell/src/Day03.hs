module Day03 where

square :: Int -> [Int]
square 0 = [1]
square n =
  let prev_square = square (n - 1) in
    let a = last prev_square + 1 in
      [a..a + (8 * 2 ^ (n - 1)) - 1]
