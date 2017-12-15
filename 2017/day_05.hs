increment :: Int -> Int -> [Int] -> [Int]
increment pos offset list =
  let (a, z) = splitAt pos list in a ++ (head z + offset : tail z)

plus1 :: Int -> Int
plus1 _ = 1

pom :: Int -> Int
pom n | n >= 3 = -1
pom _ = 1

move :: Int -> Int -> (Int -> Int) -> [Int] -> Int
move pos n f l =
  let offset = l !! pos
      next_pos = pos + offset in
    if next_pos `elem` [0..length l - 1]
    then
      let incr = f offset in move next_pos (n + 1) f (increment pos incr l)
    else n + 1

--

main :: IO ()
main = do { input <- readFile "day_05.input"; print $ move 0 0 plus1 $ map read $ lines input }

