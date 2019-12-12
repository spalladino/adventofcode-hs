{-# LANGUAGE NamedFieldPuns #-}

module Day05
    ( day05,
      intcode,
      intcodeStep,
      parseInstruction,
      Execution(..),
      Program
    ) where

import Utils.Parse (readCSList)
import Utils.Data
import Data.List
import Data.List.Split

data Opcode = 
  OPSum | OPMul | OPInput | OPOutput | OPJumpIfTrue | OPJumpIfFalse | OPLessThan | OPEquals | OPHalt 
  deriving (Eq, Show)

data ParameterMode = Position | Immediate deriving (Eq, Show)
type ParameterValue = Int
type Parameter = (ParameterMode, ParameterValue)
type Parameters = [Parameter]

data Instruction = 
  Instr { opcode :: Opcode, params :: Parameters } 
  deriving (Eq, Show)

type Program = [Int]
type Input = [Int]
type Output = [Int]
type IP = Int

data Execution = 
  Exec { program :: Program, ip :: IP, input :: Input, output :: Output } 
  | End { program :: Program, output :: Output } 
  deriving (Eq, Show)

-- |https://adventofcode.com/2019/day/5
day05 :: IO ()
day05 = interact $ show . output . intcode [5] . readCSList

intcode :: Input -> Program -> Execution
intcode input prg = head $ dropWhile isRunning $ iterate intcodeStep initialState
  where initialState = Exec prg 0 input []

isRunning :: Execution -> Bool
isRunning Exec{} = True
isRunning End{} = False

intcodeStep :: Execution -> Execution
intcodeStep exec = runInstruction instruction exec
  where instruction = parseInstruction exec

runInstruction :: Instruction -> Execution -> Execution

runInstruction (Instr OPHalt _) Exec {program, output} = 
  End program output

runInstruction instr@(Instr OPInput [(_, pos)]) exec@Exec { program, input } = 
  advanceIP instr exec { 
    program = setPos pos (head input) program,
    input = tail input }

runInstruction instr@(Instr OPOutput [param]) exec@Exec{ program, output } = 
  advanceIP instr exec {
    output = output ++ [solveParam program param] }

runInstruction instr@Instr{opcode=OPSum} exec =
  runArithmeticInstruction (+) instr exec

runInstruction instr@Instr{opcode=OPMul} exec =
  runArithmeticInstruction (*) instr exec
  
runInstruction instr@Instr{opcode=OPJumpIfTrue} exec = 
  runJumpInstruction (/=0) instr exec

runInstruction instr@Instr{opcode=OPJumpIfFalse} exec = 
  runJumpInstruction (==0) instr exec

runInstruction instr@Instr{opcode=OPLessThan} exec = 
  runArithmeticInstruction ((toInt .) . (<)) instr exec

runInstruction instr@Instr{opcode=OPEquals} exec = 
  runArithmeticInstruction ((toInt .) . (==)) instr exec
  
runArithmeticInstruction :: (Int -> Int -> Int) -> Instruction -> Execution -> Execution
runArithmeticInstruction operation instr@(Instr opcode [op1, op2, (_,dest)]) exec@Exec{program} = 
  advanceIP instr exec {
    program = setPos dest res program } 
  where res = operation (solveParam program op1) (solveParam program op2)

runJumpInstruction :: (Int -> Bool) -> Instruction -> Execution -> Execution
runJumpInstruction check (Instr opcode [cond, dest]) exec@Exec { program, ip } =
  exec {
    ip = if check (solveParam program cond) then solveParam program dest else incrementIP opcode ip }    

readPos :: Int -> Program -> Int
readPos i prg = prg !! i

setPos :: Int -> ParameterValue -> Program -> Program
setPos i val prg = let (xs,ys) = splitAt i prg in xs ++ [val] ++ (tail ys)

solveParam :: Program -> Parameter -> Int
solveParam _ (Immediate, value) = value
solveParam prg (Position, pos) = readPos pos prg

advanceIP :: Instruction -> Execution -> Execution
advanceIP Instr{opcode} exec@Exec{ip} = exec{ ip = incrementIP opcode ip }

incrementIP :: Opcode -> IP -> IP
incrementIP opcode ip = ip + paramCount opcode + 1

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
paramCount OPJumpIfTrue = 2
paramCount OPJumpIfFalse = 2
paramCount OPLessThan = 3
paramCount OPEquals = 3
paramCount OPHalt = 0

parseOperation :: Int -> (Opcode, [ParameterMode])
parseOperation x = (toOpcode (mod x 100), toModes (div x 100))

toOpcode :: Int -> Opcode
toOpcode 1 = OPSum
toOpcode 2 = OPMul
toOpcode 3 = OPInput
toOpcode 4 = OPOutput
toOpcode 5 = OPJumpIfTrue
toOpcode 6 = OPJumpIfFalse
toOpcode 7 = OPLessThan
toOpcode 8 = OPEquals
toOpcode 99 = OPHalt
toOpcode x = error ("Unexpected opcode " ++ show x)

toModes :: Int -> [ParameterMode]
toModes 0 = repeat Position
toModes x = toMode (mod x 10) : toModes (div x 10)

toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

