module Day08
  ( part1
  , part2
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)

import MyParser

data Operator
  = GT
  | LT
  | GE
  | LE
  | EQ
  | NE
  deriving (Show)

data Operation
  = INC
  | DEC

type Register = String

type Condition = (Register, Operator, Int)

type Instruction = (Register, Operation, Int, Condition)

type Program = [Instruction]

type Registers = Map.Map Register Int

program :: Parser Program
program = do
  instructions <- many instruction
  eof
  return instructions

instruction :: Parser Instruction
instruction = do
  reg <- register
  space
  ope <- operation
  space
  val <- number
  con <- condition
  endOfLine
  return (reg, ope, val, con)

register :: Parser Register
register = many letter

operation :: Parser Operation
operation =
  do string "inc"
     return INC
     <|> do
    string "dec"
    return DEC

condition :: Parser Condition
condition = do
  string " if "
  reg <- register
  space
  ope <- operator
  space
  val <- number
  return (reg, ope, val)

operator :: Parser Operator
operator =
  do try (string ">=")
     return Day08.GE
     <|> do
    try (string "<=")
    return Day08.LE <|> do
      try (string "==")
      return Day08.EQ <|> do
        try (string "!=")
        return Day08.NE <|> do
          try (string ">")
          return Day08.GT <|> do
            try (string "<")
            return Day08.LT

updateRegister :: Registers -> Register -> Operation -> Int -> Registers
updateRegister regs reg ope val =
  case Map.lookup reg regs of
    Nothing -> Map.insert reg (o 0 val) regs
    Just x -> updateWithMax regs reg (o x val) --Map.adjust (\n -> o n val) reg regs --
  where
    o =
      case ope of
        INC -> (+)
        DEC -> (-)

updateWithMax :: Registers -> Register -> Int -> Registers
updateWithMax regs reg new =
  if fromJust (Map.lookup "max" regs) < new
    then Map.insert "max" new $ Map.insert reg new regs
    else Map.insert reg new regs

maxReg :: Registers -> Int
maxReg r = maximum $ Map.elems r

exec :: Program -> Int
exec p =
  let final_state = foldl process (Map.singleton "max" 0) p
   in maxReg $ Map.delete "max" final_state

execWithMax :: Program -> Int
execWithMax p =
  let final_state = foldl process (Map.singleton "max" 0) p
   in fromJust $ Map.lookup "max" final_state

process :: Registers -> Instruction -> Registers
process r (reg, ope, val, cond)
  | test cond r = updateRegister r reg ope val
  | otherwise = r

test :: Condition -> Registers -> Bool
test (reg, ope, val) r = op ope reg_value val
  where
    reg_value = fromMaybe 0 (Map.lookup reg r)

op :: Operator -> (Int -> Int -> Bool)
op Day08.GT = (>)
op Day08.LT = (<)
op Day08.GE = (>=)
op Day08.LE = (<=)
op Day08.EQ = (==)
op Day08.NE = (/=)

-- export
part1 :: String -> String
part1 input = show $ exec $ parseWith program input

part2 :: String -> String
part2 input = show $ execWithMax $ parseWith program input
