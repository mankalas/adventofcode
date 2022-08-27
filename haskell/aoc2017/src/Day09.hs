module Day09
  ( part1
  , part2
  ) where

parseStream :: String -> Int
parseStream input = parseStream_ input [] False 0

parseStream_ :: String -> [Int] -> Bool -> Int -> Int
parseStream_ [] l _ _ = sum l
parseStream_ (x:xs) l g i =
  case x of
    '!' -> parseStream_ (tail xs) l g i
    '{' ->
      if g
        then parseStream_ xs l g i
        else parseStream_ xs l g (i + 1)
    '<' -> parseStream_ xs l True i
    '>' -> parseStream_ xs l False i
    '}' ->
      if g
        then parseStream_ xs l g i
        else parseStream_ xs (i : l) g (i - 1)
    _ -> parseStream_ xs l g i

removeGarbage :: String -> Int
removeGarbage input = removeGarbage_ input 0 False

removeGarbage_ :: String -> Int -> Bool -> Int
removeGarbage_ [] s _ = s
removeGarbage_ (x:xs) s g =
  case x of
    '!' -> removeGarbage_ (tail xs) s g
    '<' ->
      removeGarbage_
        xs
        (if g
           then s + 1
           else s)
        True
    '>' -> removeGarbage_ xs s False
    _ ->
      removeGarbage_
        xs
        (if g
           then s + 1
           else s)
        g

-- export
part1 :: String -> String
part1 input = show $ parseStream input

part2 :: String -> String
part2 input = show $ removeGarbage input
