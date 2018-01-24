module Day07 (part1, part2) where

import Parser

import Debug.Trace

import Data.Maybe
import Data.List
import Data.List.Unique
import qualified Data.Map.Strict as Map

type LeavesDef = Map.Map String (Int, [String])

data Tree = Tree String Int [Tree] | Void deriving (Show)

treeHasName :: String -> Tree -> Bool
treeHasName s (Tree n _ c) = (n == s) || (or $ map (treeHasName s) c)

treeHasNode :: Tree -> Tree -> Bool
treeHasNode (Tree n _ c) target@(Tree s _ _) = (n == s) || (or $ map (treeHasNode target) c)

treeNode :: String -> Tree -> Maybe Tree
treeNode name t@(Tree n _ c)
  | n == name = Just t
  | null c    = Nothing
  | otherwise = find (isJust . treeNode name) c

buildTree :: LeavesDef -> Tree
buildTree m = buildTree_ m (Tree "" (-1) []) (Map.keys m)

buildTree_ :: LeavesDef -> Tree -> [String] -> Tree
buildTree_ _ t [] = t
buildTree_ m t keys@(x:xs)
  | null keys       = t
  | treeHasName x t = buildTree_ m t xs
  | otherwise       = let orphan = buildOrphan m t x
                          cleaned_tree = removeOrphans orphan t in
                        buildTree_ m cleaned_tree xs

removeOrphans :: Tree -> Tree -> Tree
removeOrphans o t@(Tree n w c) =
  Tree n w $ o:unrelated_orphans
  where unrelated_orphans = filter (not_has o) c
        not_has source target = not $ treeHasNode source target

buildOrphan :: LeavesDef -> Tree -> String -> Tree
buildOrphan m t n =
  let (weight, children) = fromMaybe (-1, []) $ Map.lookup n m in
    case treeNode n t of
      Nothing -> Tree n weight $ map (buildOrphan m t) children
      Just x  -> x

buildMap :: [Parser.LeafDef] -> LeavesDef
buildMap result = foldl (\m (n, w, c) -> Map.insert n (w, c) m) Map.empty $ result

treeWeight :: Tree -> Int
treeWeight (Tree _ w c)
 | null c = w
 | otherwise = w + (sum $ map treeWeight c)

allIdentical :: Eq a => [a] -> Bool
allIdentical l@(x:xs) = countElem x l == length l

newWeight :: Int -> Tree -> Int -> Int
newWeight source (Tree _ w _) target
 | source > target = w - source + target
 | otherwise       = w + target - source

wrongWeight :: [Int] -> Int
wrongWeight l =
  case find ((==) 1 . fst) $ occurrences l of
    Nothing       -> error("No wrong weight in " ++ show l)
    Just (x, [y]) -> y

rightWeight :: [Int] -> Int
rightWeight l =
  case find ((/=) 1 . fst) $ occurrences l of
    Nothing     -> error("There must be a right weight in " ++ show l)
    Just (x, y) -> head y

treeWithWeight :: [Tree] -> Int -> Tree
treeWithWeight l i =
  case find ((==) i . treeWeight) l of
    Nothing -> error("No tree with given weight " ++ show i ++ " found")
    Just t  -> t

fixWeight :: Tree -> Int -> Int
fixWeight t@(Tree _ w c) pw =
  let cw = map treeWeight c in
    if allIdentical cw
    then newWeight (w + sum cw) t pw
    else
      let rw = rightWeight cw
          ww = wrongWeight cw in
        fixWeight (treeWithWeight c ww) (rightWeight cw)

tree :: String -> Tree
tree input =
  let (Tree _ _ c) = buildTree $ buildMap $ Parser.parseFile input in
    head c

-- exports

part1 :: String -> String
part1 input =
  let (Tree n _ _) = tree input in n

part2 :: String -> String
part2 input =
  let t = tree input in show $ fixWeight t 0
