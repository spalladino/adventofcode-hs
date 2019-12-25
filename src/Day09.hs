{-# LANGUAGE NamedFieldPuns #-}

module Day09
    ( day09,
      intcode,
      intcodeRun,
      intcodeStep,
      initialState,
      readProgram,
      parseInstruction,
      isRunning,
      isEnded,
      pushInput,
      consumeOutput,
      Execution(..),
      Program,
      Input,
      Output
    ) where

import Utils.Parse (readCSList)
import Utils.Data
import Data.List
import Data.List.Split

import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed ((!), (!?), (//), replicate, fromList, toList, Vector)
import qualified Data.Vector.Unboxed as Vector

data Opcode = 
  OPSum | OPMul | OPInput | OPOutput | OPJumpIfTrue | OPJumpIfFalse | OPLessThan | OPEquals | OPSetRelativeBase | OPHalt 
  deriving (Eq, Show)

data ParameterMode = Position | Immediate | Relative deriving (Eq, Show)
type ParameterValue = Int
type Parameter = (ParameterMode, ParameterValue)
type Parameters = [Parameter]

data Instruction = 
  Instr { opcode :: Opcode, params :: Parameters } 
  deriving (Eq, Show)

type Memory = Vector Int
type Program = [Int]
type Input = [Int]
type Output = [Int]
type IP = Int
type RP = Int

data State = Running | AwaitingInput | Ended
  deriving (Eq, Show)

data Execution = 
  Exec { state :: State, memory :: Memory, ip :: IP, relativeBase :: RP, input :: Input, output :: Output } 
  | NilExecution
  deriving (Eq, Show)

-- |https://adventofcode.com/2019/day/9
day09 :: IO ()
day09 = interact $ show . output . intcode [2] . readProgram

readProgram :: String -> Program
readProgram = readCSList

intcode :: Input -> Program -> Execution
intcode input program = intcodeRun input (initialState program [])

intcodeRun :: Input -> Execution -> Execution
intcodeRun input state = head $ dropWhile isRunning $ iterate intcodeStep $ pushInput state input

initialState :: Program -> Input -> Execution
initialState program input = Exec { state=Running, memory=fromList program, ip=0, relativeBase=0, input, output=[] }

isRunning :: Execution -> Bool
isRunning Exec{ state=Running } = True
isRunning _ = False

isEnded :: Execution -> Bool
isEnded Exec{ state=Ended } = True
isEnded _ = False

intcodeStep :: Execution -> Execution
intcodeStep exec = runInstruction instruction exec
  where instruction = parseInstruction exec

pushInput :: Execution -> Input -> Execution
pushInput exec [] = exec
pushInput exec@Exec{ state, input } pushed = exec{ state=newState, input=input++pushed }
  where newState = case state of Running -> Running
                                 AwaitingInput -> Running
                                 Ended -> Ended

consumeOutput :: Execution -> (Execution, Output)
consumeOutput exec@Exec{ output } = (exec{ output=[] }, output)

runInstruction :: Instruction -> Execution -> Execution
runInstruction _ exec@Exec{ state=Ended } = 
  error "Cannot run instruction on ended execution"

runInstruction (Instr OPHalt _) exec@Exec{} = 
  exec{ state=Ended }

runInstruction instr@(Instr OPInput _) exec@Exec { state=Running, input=[] } = 
  exec{ state=AwaitingInput }

runInstruction instr@(Instr OPInput [param]) exec@Exec { state=Running, memory, input } = 
  advanceIP instr (setMem param (head input) exec) { input = tail input }

runInstruction instr@(Instr OPOutput [param]) exec@Exec{ state=Running, output } = 
  advanceIP instr exec { output = output ++ [solveParam exec param] }

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

runInstruction instr@(Instr OPSetRelativeBase [param]) exec@Exec{ state=Running, relativeBase } = 
  advanceIP instr exec { relativeBase = relativeBase + solveParam exec param }

runArithmeticInstruction :: (Int -> Int -> Int) -> Instruction -> Execution -> Execution
runArithmeticInstruction operation instr@(Instr opcode [op1, op2, dest]) exec@Exec{ state=Running, memory } = 
  advanceIP instr (setMem dest res exec)
  where res = operation (solveParam exec op1) (solveParam exec op2)

runJumpInstruction :: (Int -> Bool) -> Instruction -> Execution -> Execution
runJumpInstruction check (Instr opcode [cond, dest]) exec@Exec{ state=Running, ip } =
  exec { ip = newIP }    
  where newIP = if check (solveParam exec cond) 
                then solveParam exec dest 
                else incrementIP opcode ip

readMem :: Int -> Memory -> Int
readMem i mem = fromMaybe 0 (mem !? i)

setMem :: Parameter -> Int -> Execution -> Execution
setMem (Relative, pos) value exec@Exec{ memory, relativeBase } = exec{ memory=setMemAt (relativeBase + pos) value memory }
setMem (_, pos) value exec@Exec{ memory } = exec{ memory=setMemAt pos value memory }

setMemAt :: Int -> Int -> Memory -> Memory
setMemAt pos val mem = newMem // [(pos,val)]
    where newMem = if pos < currSize then mem else mem Vector.++ Vector.replicate extSize 0
          currSize = Vector.length mem
          extSize = (pos - currSize + 1) * 2

solveParam :: Execution -> Parameter -> Int
solveParam _ (Immediate, value) = value
solveParam Exec{ memory } (Position, pos) = readMem pos memory
solveParam Exec{ memory, relativeBase } (Relative, pos) = readMem (pos + relativeBase) memory

advanceIP :: Instruction -> Execution -> Execution
advanceIP Instr{opcode} exec@Exec{ip} = exec{ ip = incrementIP opcode ip }

incrementIP :: Opcode -> IP -> IP
incrementIP opcode ip = ip + paramCount opcode + 1

parseInstruction :: Execution -> Instruction
parseInstruction Exec{ memory, ip } = instruction
  where instruction = makeInstruction op (toList $ Vector.slice (ip+1) (paramCount opcode) memory)
        op@(opcode,_) = parseOperation (readMem ip memory)
    
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
paramCount OPSetRelativeBase = 1
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
toOpcode 9 = OPSetRelativeBase
toOpcode 99 = OPHalt
toOpcode x = error ("Unexpected opcode " ++ show x)

toModes :: Int -> [ParameterMode]
toModes 0 = repeat Position
toModes x = toMode (mod x 10) : toModes (div x 10)

toMode :: Int -> ParameterMode
toMode 0 = Position
toMode 1 = Immediate
toMode 2 = Relative

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

