module Day07 (part1, part2) where

import Debug.Trace

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

type LeafDef = (String, Int, [String])
type LeavesDef = Map.Map String (Int, [String])

data Tree = Tree String Int [Tree] | Void deriving (Show)

treeHasName :: Tree -> String -> Bool
treeHasName Void _ = False
treeHasName (Tree n _ c) s = (n == s) || (or $ map (\t -> treeHasName t s) c)

treeHasNode :: Tree -> Tree -> Bool
treeHasNode Void _ = False
treeHasNode _ Void = False
treeHasNode (Tree n _ c) t@(Tree s _ _) = (n == s) || (or $ map (treeHasNode t) c)

treeNode :: Tree -> String -> Tree
treeNode Void _ = Void
treeNode t@(Tree n _ c) name =
  if hasName name t
  then t
  else fromMaybe Void $ find (hasName name) c
  where hasName name t@(Tree n _ _) = name == n

buildTree :: LeavesDef -> Tree
buildTree m = buildTree_ m treeHolder (Map.keys m)

buildTree_ :: LeavesDef -> Tree -> [String] -> Tree
buildTree_ _ t [] = t
buildTree_ m t (x:xs) =
  if treeHasName t x
  then trace(show "Tree has " ++ x) buildTree_ m t xs
  else
    let orphan = buildOrphan m x t
        cleaned_tree = removeOrphans orphan t in
      --trace(show t)
      trace(show cleaned_tree)
      buildTree_ m cleaned_tree xs

removeOrphans :: Tree -> Tree -> Tree
removeOrphans o t@(Tree n w c) = trace(show unrelated_orphans) Tree n w $ o:unrelated_orphans
  where unrelated_orphans = filter (\child -> not $ treeHasNode o child) c

buildOrphan :: LeavesDef -> String -> Tree -> Tree
buildOrphan m n t =
  let orphan = treeNode t n
      (weight, children) = fromMaybe (-1, []) $ Map.lookup n m in
    case orphan of
      Void -> Tree n weight $ map (\child -> buildOrphan m child t) children
      _    -> orphan

buildMap :: Either ParseError [LeafDef] -> LeavesDef
buildMap result =
  if isLeft result
  then Map.empty
  else foldl (\m (n, w, c) -> Map.insert n (w, c) m) Map.empty $ fromRight [] result

parseFile :: String -> Either ParseError [LeafDef]
parseFile input = parse file "(unknown)" input

-- parsers

file :: GenParser Char st [LeafDef]
file = do
  result <- many line
  eof
  return result

line :: GenParser Char st LeafDef
line = do
  n <- many alphaNum
  _ <- char ' '
  w <- weight
  c <- children
  _ <- endOfLine
  return (n, w, c)

weight :: GenParser Char st Int
weight = do
  _ <- char '('
  w <- many digit
  _ <- char ')'
  return $ read w

children :: GenParser Char st [String]
children = do
  first <- child
  next <- remainingChildren
  if first == "" then return [] else return (first:next)

child :: GenParser Char st String
child = try ( do
                _ <- string " -> "
                c <- many (noneOf ",\n")
                return c
            ) <|> many (noneOf ",\n")

remainingChildren :: GenParser Char st [String]
remainingChildren = (string ", " >> children) <|> return []

treeHolder :: Tree
treeHolder = Tree "" (-1) []

-- exports

pick :: Tree -> String
pick (Tree _ _ (x:_)) =
  let (Tree n _ _) = x in n

tree :: String -> Tree
tree input = buildTree $ buildMap $ parseFile input

part1 :: String -> String
part1 input = show $ pick $ tree input

part2 :: String -> String
part2 input = show $ pick $ tree input
