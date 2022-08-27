module Day07
  ( part1
  , part2
  ) where

import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Text.Parsec
import Text.Parsec.String (Parser)

import MyList (countElem, occurrences)
import MyParser

data Tree
  = Tree String Int [Tree]
  | Void
  deriving (Show)

type LeavesDef = Map.Map String (Int, [String])

type LeafDef = (String, Int, [String])

-- Parser
file :: Parser [LeafDef]
file = do
  result <- many line
  eof
  return result

line :: Parser LeafDef
line = do
  n <- many alphaNum
  _ <- char ' '
  w <- weight
  c <- children
  _ <- endOfLine
  return (n, w, c)

weight :: Parser Int
weight = do
  _ <- char '('
  w <- many digit
  _ <- char ')'
  return $ read w

children :: Parser [String]
children = do
  first <- child
  next <- remainingChildren
  if first == ""
    then return []
    else return (first : next)

child :: Parser String
child =
  try
    (do _ <- string " -> "
        many (noneOf ",\n")) <|>
  many (noneOf ",\n")

remainingChildren :: Parser [String]
remainingChildren = (string ", " >> children) <|> return []

-- Tree
treeHasName :: String -> Tree -> Bool
treeHasName s (Tree n _ c) = (n == s) || any (treeHasName s) c
treeHasName _ Void = False

treeHasNode :: Tree -> Tree -> Bool
treeHasNode (Tree n _ c) target@(Tree s _ _) =
  (n == s) || any (treeHasNode target) c
treeHasNode _ _ = False

treeNode :: String -> Tree -> Maybe Tree
treeNode name t@(Tree n _ c)
  | n == name = Just t
  | null c = Nothing
  | otherwise = find (isJust . treeNode name) c
treeNode _ Void = Nothing

buildTree :: LeavesDef -> Tree
buildTree m = buildTree_ m (Tree "" (-1) []) (Map.keys m)

buildTree_ :: LeavesDef -> Tree -> [String] -> Tree
buildTree_ _ t [] = t
buildTree_ m t keys@(x:xs)
  | null keys = t
  | treeHasName x t = buildTree_ m t xs
  | otherwise =
    let orphan = buildOrphan m t x
        cleaned_tree = removeOrphans orphan t
     in buildTree_ m cleaned_tree xs

removeOrphans :: Tree -> Tree -> Tree
removeOrphans o (Tree n w c) = Tree n w $ o : unrelated_orphans
  where
    unrelated_orphans = filter (not_has o) c
    not_has source target = not $ treeHasNode source target
removeOrphans _ _ = Void

buildOrphan :: LeavesDef -> Tree -> String -> Tree
buildOrphan m t n =
  let (w, c) = fromMaybe (-1, []) $ Map.lookup n m
   in case treeNode n t of
        Nothing -> Tree n w $ map (buildOrphan m t) c
        Just x -> x

buildMap :: [LeafDef] -> LeavesDef
buildMap = foldl (\m (n, w, c) -> Map.insert n (w, c) m) Map.empty

treeWeight :: Tree -> Int
treeWeight (Tree _ w c)
  | null c = w
  | otherwise = w + sum (map treeWeight c)
treeWeight Void = 0

allIdentical :: Eq a => [a] -> Bool
allIdentical l@(x:_) = countElem x l == length l
allIdentical [] = True

newWeight :: Int -> Tree -> Int -> Int
newWeight source (Tree _ w _) target
  | source > target = w - source + target
  | otherwise = w + target - source
newWeight _ _ _ = 0

wrongWeight :: [Int] -> Int
wrongWeight l =
  case find ((==) 1 . fst) $ occurrences l of
    Nothing -> error ("No wrong weight in " ++ show l)
    Just (_, [y]) -> y
    Just _ -> error "Wrong weight"

rightWeight :: [Int] -> Int
rightWeight l =
  case find ((/=) 1 . fst) $ occurrences l of
    Nothing -> error ("There must be a right weight in " ++ show l)
    Just (_, y) -> head y

treeWithWeight :: [Tree] -> Int -> Tree
treeWithWeight l i =
  case find ((==) i . treeWeight) l of
    Nothing -> error ("No tree with given weight " ++ show i ++ " found")
    Just t -> t

fixWeight :: Tree -> Int -> Int
fixWeight t@(Tree _ w c) pw =
  let cw = map treeWeight c
   in if allIdentical cw
        then newWeight (w + sum cw) t pw
        else let ww = wrongWeight cw
              in fixWeight (treeWithWeight c ww) (rightWeight cw)
fixWeight _ _ = 0

tree :: String -> Tree
tree input =
  let (Tree _ _ c) = buildTree $ buildMap $ parseWith file input
   in head c

-- exports
part1 :: String -> String
part1 input =
  let (Tree n _ _) = tree input
   in n

part2 :: String -> String
part2 input =
  let t = tree input
   in show $ fixWeight t 0
