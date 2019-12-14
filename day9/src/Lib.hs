module Lib
    ( day9
    , runProgram
    , Program(..)
    , Param(..)
    , ParamMode(..)
    , parseOpcode
    , parseOp
    , fetchOp
    , executeOp
    , step
    , writeMemory
    ) where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Sequence (Seq, (<|), (!?), (|>))
import qualified Data.Sequence as Seq

type Memory = [Int]

type Input = Seq Int

type Output = Seq Int

type Pc = Int

type RelBase = Int

data Program = Running Memory Input Output Pc RelBase
    | Halted Memory Output
    | Error Memory Pc deriving Show

type RunningProgram = State Program ()

type OutProgram = State Program (Maybe Int)

data ParamMode = PosMode | ImmediateMode | RelativeMode deriving Show

data Param = Param Int ParamMode deriving Show

type Opcode = Int

type RawOpcode = Int

data Op =
      Add Param Param Param
    | Mul Param Param Param
    | Input Param
    | Output Param
    | JumpIfTrue Param Param
    | JumpIfFalse Param Param
    | LessThan Param Param Param
    | Equals Param Param Param
    | AdjustRelBase Param
    | End deriving Show

oneParamOps = ['3', '4', '9']

twoParamOps = ['5', '6']

threeParamOps = ['1', '2', '7', '8']

parseExtended digit modes
        | digit `elem` threeParamOps =
            -- see Traversable instance of (,) a to see why this works
            sequence ((read [digit]), (parseModes_ 3 modes))
        | digit `elem` twoParamOps =
            -- see Traversable instance of (,) a to see why this works
            sequence ((read [digit]), (parseModes_ 2 modes))
        | digit `elem` oneParamOps =
            -- see Traversable instance of (,) a to see why this works
            sequence ((read [digit]), (parseModes_ 1 modes))
        | otherwise = Nothing

parseBase digit
        | digit `elem` threeParamOps =
            return ((read [digit]) , replicate 3 PosMode)
        | digit `elem` twoParamOps =
            return ((read [digit]) , replicate 2 PosMode)
        | digit `elem` oneParamOps =
            return ((read [digit]) , replicate 1 PosMode)
        | otherwise = Nothing

parseOpcode :: Int -> Maybe (RawOpcode, [ParamMode])
parseOpcode opcode = 
    case reverse . show $ opcode of
        digit : '0' : modes -> parseExtended digit modes
        digit : [] -> parseBase digit
        _ -> Nothing

parseModes_ :: Int -> String -> Maybe [ParamMode]
parseModes_ paramCnt chrs =
    let mode '2' = Just RelativeMode  
        mode '1' = Just ImmediateMode  
        mode '0' = Just PosMode  
        mode _ = Nothing
        res = take paramCnt $ (map mode chrs) ++ (repeat $ Just PosMode)
    in sequence res

opFromParams3 :: (Param -> Param -> Param -> Op) -> [Param] -> Op
opFromParams3 opC params = opC (params !! 0) (params !! 1) (params !! 2)

opFromParams2 :: (Param -> Param -> Op) -> [Param] -> Op
opFromParams2 opC params = opC (params !! 0) (params !! 1)

opFromParams :: (Param -> Op) -> [Param] -> Op
opFromParams opC params = opC (params !! 0)

parseOp :: Program -> Maybe (Op, Int)
parseOp (Running (99 : _) _ _ _ _) = Just (End, 0)
parseOp (Running (opcode : rst) _ _ _ _) = do
    let buildParams n = map (uncurry Param) . zip (take n rst)
    parsed <- parseOpcode opcode
    case parsed of
        (1, modes) -> Just ((opFromParams3 Add (buildParams 3 modes)), 4)
        (2, modes) -> Just ((opFromParams3 Mul (buildParams 3 modes)), 4)
        (3, modes) -> Just ((opFromParams Input (buildParams 1 modes)), 2)
        (4, modes) -> Just ((opFromParams Output (buildParams 1 modes)), 2)
        (5, modes) -> Just ((opFromParams2 JumpIfTrue (buildParams 2 modes)), 3)
        (6, modes) -> Just ((opFromParams2 JumpIfFalse (buildParams 2 modes)), 3)
        (7, modes) -> Just ((opFromParams3 LessThan (buildParams 3 modes)), 4)
        (8, modes) -> Just ((opFromParams3 Equals (buildParams 3 modes)), 4)
        (9, modes) -> Just ((opFromParams AdjustRelBase (buildParams 1 modes)), 2)
        (_) -> Nothing
parseOp (_) = Nothing

fetchOp :: Memory -> Param -> Int -> Int
fetchOp mem op rb = case op of
    (Param val ImmediateMode) -> val
    (Param val PosMode) -> if (val > length mem) then 0 else mem !! val
    (Param val RelativeMode) ->
        if (addr > length mem) then 0 else mem !! addr
        where
            addr = rb + val

fetchOpVal :: Param -> Int
fetchOpVal (Param val _) = val

fetchAddr :: Param -> Int -> Maybe Int
fetchAddr param rb = case param of
        Param val RelativeMode -> Just $ rb + val
        Param val PosMode -> Just val
        _ -> Nothing

maybeGrowMemory :: Memory -> Int -> Memory
maybeGrowMemory mem addr =
    if (length mem) < addr + 1
    then mem ++ (take extension $ repeat 0)
    else mem
    where
        extension = addr + 1 - length mem

writeMemory :: Memory -> Int -> Int -> Memory
writeMemory mem ptr val = beg ++ [val] ++ tail end
    where
        grownMem = maybeGrowMemory mem ptr
        (beg, end) = splitAt ptr grownMem

executeOp :: Program -> Op -> Maybe (Program, Maybe Int)
executeOp (Running mem i o pc rb) (Add fst snd ptr) = do
    addr <- fetchAddr ptr rb
    Just ((Running (writeMemory mem addr (op1 + op2)) i o pc rb), Nothing)
    where
        op1 = fetchOp mem fst rb
        op2 = fetchOp mem snd rb
executeOp (Running mem i o pc rb) (Mul fst snd ptr) = do 
    addr <- fetchAddr ptr rb
    Just ((Running (writeMemory mem addr (op1 * op2)) i o pc rb), Nothing)
    where
        op1 = fetchOp mem fst rb
        op2 = fetchOp mem snd rb
executeOp (Running mem i o pc rb) (Input ptr) = do
    input <- i !? 0
    addr <- fetchAddr ptr rb
    Just ((Running (writeMemory mem addr input) (Seq.drop 1 i) o pc rb), Nothing)
executeOp (Running mem i o pc rb) (Output fst) =
    Just ((Running mem i ((fetchOp mem fst rb) <| o) pc rb), Nothing)
executeOp (Running mem i o pc rb) (JumpIfTrue fst snd) =
    if (fetchOp mem fst rb) /= 0
        then Just ((Running mem i o pc rb), Just $ fetchOp mem snd rb)
        else Just ((Running mem i o pc rb), Nothing)
executeOp (Running mem i o pc rb) (JumpIfFalse fst snd) =
    if (fetchOp mem fst rb) == 0
        then Just ((Running mem i o pc rb), Just $ fetchOp mem snd rb)
        else Just ((Running mem i o pc rb), Nothing)
executeOp (Running mem i o pc rb) (LessThan fst snd ptr) = do
    addr <- fetchAddr ptr rb
    Just ((Running (writeMemory mem addr val) i o pc rb), Nothing)
    where
        val = if (fetchOp mem fst rb) < (fetchOp mem snd rb) then 1 else 0
executeOp (Running mem i o pc rb) (Equals fst snd ptr) = do
    addr <- fetchAddr ptr rb
    Just ((Running (writeMemory mem addr val) i o pc rb), Nothing)
    where
        val = if (fetchOp mem fst rb) == (fetchOp mem snd rb) then 1 else 0
executeOp (Running mem i o pc rb) (AdjustRelBase ptr) =
    Just $ (Running mem i o pc (rb + (fetchOp mem ptr rb)), Nothing)
executeOp (Running mem i o pc rb) End = Just ((Halted mem o), Nothing)

rewindProgram :: Program -> Program
rewindProgram (Running prog i o pc rb) = Running (drop pc prog) i o 0 rb
rewindProgram (non_running) = non_running

movePc :: Program -> Int -> Program
movePc (Running prog i o pc rb) new_pc = Running prog i o new_pc rb
movePc (non_running) ptr = non_running

getPc :: Program -> Maybe Int
getPc (Running prog i o pc rb) = Just pc
getPc (non_running) = Nothing

step :: RunningProgram
step = do
    prog <- get
    case prog of
        Halted mem o -> put $ Halted mem o
        Running mem i o pc rb -> 
            case execute_ prog of
                Nothing -> put $ Error mem pc
                Just (prog, jmp) -> put $ movePc prog jmp
        Error mem pc -> put $ Error mem pc
    where 
        execute_ prog = do
            ptr <- getPc prog
            (op, op_len) <- parseOp (rewindProgram prog) 
            mod_prog <- executeOp prog op
            case mod_prog of
                (p, Nothing) -> return (p, ptr + op_len)
                (p, Just new_ptr) -> return (p, new_ptr) 

runUntil :: (Program -> Bool) -> RunningProgram
runUntil pred = do
    prog <- get
    if pred prog
        then return ()
        else step >> (runUntil pred)

interpret :: RunningProgram
interpret = runUntil done_
    where
        done_ (Halted m o) = True
        done_ (Error m ptr) = True
        done_ _ = False

loadProgram :: IO Memory
loadProgram = liftM ((map read) . splitOn ",") . readFile $ "input.txt"

runProgram :: Program -> Program
runProgram program = execState interpret program

passInput :: Int -> Program -> Program
passInput val (Running m i o pc rb) = Running m (i |> val) o pc rb
passInput val prog = prog

readOutput :: Program -> Maybe Int
readOutput (Halted mem o) = o !? 0
readOutput (_) = Nothing

getLastOutput :: Program -> Maybe Int
getLastOutput (Running mem i o pc rb) = o !? 0
getLastOutput (Halted mem o) = o !? 0
getLastOutput (_) = Nothing

validateResult :: Program -> IO ()
validateResult (Halted mem o) = print mem >> print o
validateResult (error) = print error

dropOutput :: Program -> Program
dropOutput (Running mem i o pc rb) = Running mem i Seq.empty pc rb
dropOutput prog = prog

initProgram :: Memory -> Int -> Program
initProgram mem input = Running mem (Seq.singleton input) Seq.empty 0 0

day9 :: IO ()
day9 = do
    mem <- loadProgram
    validateResult $ runProgram $ initProgram mem 1
