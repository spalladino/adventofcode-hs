module Day05
    ( day05,
      intcode,
      intcodeStep,
      parseInstruction,
      Execution(..)
    ) where

import Utils.Parse
import Utils.Data
import Data.List
import Data.List.Split

data Opcode = OPSum | OPMul | OPInput | OPOutput | OPHalt deriving (Eq, Show)
data ParameterMode = Position | Immediate deriving (Eq, Show)
type ParameterValue = Int
type Parameter = (ParameterMode, ParameterValue)
type Parameters = [Parameter]
data Instruction = Instr Opcode Parameters deriving (Eq, Show)

type Program = [Int]
type Input = [Int]
type Output = [Int]
type IP = Int
data Execution = Exec Program IP Input Output | End Program Output deriving (Eq, Show)

-- |https://adventofcode.com/2019/day/5
day05 :: IO ()
day05 = interact $ show . getOutput . intcode [1] . readCSList

getOutput :: Execution -> Output
getOutput (End _ output) = output

intcode :: Input -> Program -> Execution
intcode input prg = head $ dropWhile isRunning $ iterate intcodeStep initialState
  where initialState = Exec prg 0 input []

isRunning :: Execution -> Bool
isRunning (Exec _ _ _ _) = True
isRunning (End _ _) = False

intcodeStep :: Execution -> Execution
intcodeStep exec = runInstruction instruction exec
  where instruction = parseInstruction exec

runInstruction :: Instruction -> Execution -> Execution

runInstruction (Instr OPHalt _) (Exec prg _ _ output) = End prg output

runInstruction (Instr OPInput [(_, pos)]) (Exec prg ip input output) = 
  Exec (setPos pos (head input) prg) (advanceIP OPInput ip) (tail input) output

runInstruction (Instr OPOutput [param]) (Exec prg ip input output) = 
  Exec prg (advanceIP OPOutput ip) input (output ++ [val])
  where val = solveParam prg param

runInstruction (Instr OPSum [op1, op2, (_,dest)]) (Exec prg ip input output) = 
  Exec (setPos dest res prg) (advanceIP OPSum ip) input output
  where res = (solveParam prg op1) + (solveParam prg op2)

runInstruction (Instr OPMul [op1, op2, (_,dest)]) (Exec prg ip input output) = 
  Exec (setPos dest res prg) (advanceIP OPMul ip) input output
  where res = (solveParam prg op1) * (solveParam prg op2)

readPos :: Int -> Program -> Int
readPos i prg = prg !! i

setPos :: Int -> ParameterValue -> Program -> Program
setPos i val prg = let (xs,ys) = splitAt i prg in xs ++ [val] ++ (tail ys)

solveParam :: Program -> Parameter -> Int
solveParam _ (Immediate, value) = value
solveParam prg (Position, pos) = readPos pos prg

advanceIP :: Opcode -> IP -> IP
advanceIP opcode ip = ip + paramCount opcode + 1

parseInstruction :: Execution -> Instruction
parseInstruction (Exec prg ip input output) = instruction
  where instruction = makeInstruction (parseOperation (prg !! ip)) (drop (ip+1) prg)
    
makeInstruction :: (Opcode, [ParameterMode]) -> [Int] -> Instruction
makeInstruction (opcode, modes) xs = Instr opcode params
  where params = zip modes (take n xs)
        n = paramCount opcode

paramCount :: Opcode -> Int
paramCount OPSum = 3
paramCount OPMul = 3
paramCount OPInput = 1
paramCount OPOutput = 1
paramCount OPHalt = 0

parseOperation :: Int -> (Opcode, [ParameterMode])
parseOperation x = (toOpcode (mod x 100), toModes (div x 100))

toOpcode :: Int -> Opcode
toOpcode 1 = OPSum
toOpcode 2 = OPMul
toOpcode 3 = OPInput
toOpcode 4 = OPOutput
toOpcode 99 = OPHalt
toOpcode x = error ("Unexpected opcode " ++ show x)

toModes :: Int -> [ParameterMode]
toModes 0 = repeat Position
toModes x = toMode (mod x 10) : toModes (div x 10)

toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate



