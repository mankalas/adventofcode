module Day16 (part1, part2) where

import Common
import Cheat

import Debug.Trace

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split

data Move = Spin Int | Exchange Int Int | Partner Char Char

max_progs = 16
progs = map (chr . (+) 97) [0..max_progs - 1]

spin :: Int -> String -> String
spin n = take max_progs . drop (max_progs - n) . cycle

exchange :: Int -> Int -> String -> String
exchange a b p = swapElts a b p

partner :: Char -> Char -> String -> String
partner c1 c2 p = swapElts (fromJust $ elemIndex c1 p) (fromJust $ elemIndex c2 p) p

step :: String -> Move -> String
step i m =
  case m of
    Spin n -> spin n i
    Exchange x y -> exchange x y i
    Partner a b -> partner a b i

danse :: [Move] -> String
danse moves = danse_ progs moves

danse_ :: String -> [Move] -> String
danse_ p [] = p
danse_ p (m:ms) = danse_ (step p m) ms

parseToken :: String -> Move
parseToken (x:xs) =
  case x of
    's' -> Spin ((read xs) :: Int)
    'x' ->
      let args = map (\x -> read x :: Int) $ splitOn "/" xs in
        Exchange (head args) (last args)
    'p' ->
      let args = splitOn "/" xs in
        Partner (head $ head args) (head $ last args)

parse :: String -> [Move]
parse input =
  let tokens = splitOn "," input in map parseToken tokens

-- export

part1 :: String -> String
part1 = danse . parse

part2 :: String -> String
part2 _ = "Bob"
