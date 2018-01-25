module Day08 (part1, part2) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Char
import Control.Monad (ap)

import Debug.Trace

import Data.Maybe
import Data.Either.Unwrap
import qualified Data.Map.Strict as Map

data Operator = GT | LT | GE | LE | EQ | NE deriving ( Show )
data Operation = INC | DEC
type Register = String
type Condition = (Register, Operator, Int)
type Instruction = (Register, Operation, Int, Condition)
type Program = [Instruction]
type Registers = Map.Map Register Int

program :: GenParser Char st Program
program = do
  instructions <- many instruction
  eof
  return instructions

instruction :: GenParser Char st Instruction
instruction = do
  reg <- register
  _   <- space
  ope <- operation
  _   <- space
  val <- int
  con <- condition
  _   <- endOfLine
  return (reg, ope, val, con)

register :: GenParser Char st Register
register = many letter

operation :: GenParser Char st Operation
operation =
  do { string "inc"; return INC } <|>
  do { string "dec"; return DEC }

int :: GenParser Char st Int
int = ap sign nat

sign :: Num a => CharParser st (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

nat :: CharParser st Int
nat =
  do { char '0' >> return 0 } <|>
  do { n <- many1 digit; return (read n) }

condition :: GenParser Char st Condition
condition = do
  _   <- string " if "
  reg <- register
  _   <- space
  ope <- operator
  _   <- space
  val <- int
  return (reg, ope, val)

operator :: GenParser Char st Operator
operator =
  do { try (string ">="); return Day08.GE } <|>
  do { try (string "<="); return Day08.LE } <|>
  do { try (string "=="); return Day08.EQ } <|>
  do { try (string "!="); return Day08.NE } <|>
  do { try (string ">" ); return Day08.GT } <|>
  do { try (string "<" ); return Day08.LT }

updateRegister :: Registers -> Register -> Operation -> Int -> Registers
updateRegister regs reg ope val =
  case Map.lookup reg regs of
    Nothing -> Map.insert reg (o 0 val) regs
    Just _  -> Map.adjust (\n -> o n val) reg regs
  where o = case ope of
          INC -> (+)
          DEC -> (-)

maxReg :: Registers -> Int
maxReg r = maximum $ Map.elems r

exec :: Program -> Int
exec p =
  let final_state = foldl process Map.empty p in
    maxReg final_state

process :: Registers -> Instruction -> Registers
process r (reg, ope, val, cond)
  | test cond r = updateRegister r reg ope val
  | otherwise   = r

test :: Condition -> Registers -> Bool
test (reg, ope, val) r =
  op(ope) reg_value val
  where reg_value = case Map.lookup reg r of
                      Nothing -> 0
                      Just n  -> n

op :: Operator -> (Int -> Int -> Bool)
op Day08.GT = (>)
op Day08.LT = (<)
op Day08.GE = (>=)
op Day08.LE = (<=)
op Day08.EQ = (==)
op Day08.NE = (/=)

parseProgram :: String -> Program
parseProgram input =
  let p = parse program "(unknown)" input in
    if isLeft p
    then error(show $ fromLeft p)
    else fromRight p

-- export

part1 :: String -> String
part1 input = show $ exec $ parseProgram input

part2 :: String -> String
part2 input = "42"
