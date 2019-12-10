module Lib
    ( day7
    , runProgram
    , Program(..)
    , parseOpcode
    , parseOp
    ) where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Sequence (Seq, (<|), (!?))
import qualified Data.Sequence as Seq

type Memory = [Int]

type Input = Seq Int

type Output = Seq Int

data Program = Running Memory Input Output | Halted Memory Output | Error Memory Int deriving Show

type RunningProgram = State Program ()

data ParamMode = PosMode | ImmediateMode deriving Show

data Param = Param Int ParamMode deriving Show

type Opcode = Int

type RawOpcode = Int

data Op =
      Add Param Param Param
    | Mul Param Param Param
    | Input Int
    | Output Param
    | JumpIfTrue Param Param
    | JumpIfFalse Param Param
    | LessThan Param Param Param
    | Equals Param Param Param
    | End deriving Show

oneParamOps = ['4']

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
    let mode '1' = Just ImmediateMode  
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
parseOp (Running (99 : _) _ _) = Just (End, 0)
parseOp (Running (3 : ptr : _) _ _) = Just (Input ptr, 2)
parseOp (Running (opcode : rst) _ _) = do
    let buildParams n = map (uncurry Param) . zip (take n rst)
    parsed <- parseOpcode opcode
    case parsed of
        (1, modes) -> Just ((opFromParams3 Add (buildParams 3 modes)), 4)
        (2, modes) -> Just ((opFromParams3 Mul (buildParams 3 modes)), 4)
        (4, modes) -> Just ((opFromParams Output (buildParams 1 modes)), 2)
        (5, modes) -> Just ((opFromParams2 JumpIfTrue (buildParams 2 modes)), 3)
        (6, modes) -> Just ((opFromParams2 JumpIfFalse (buildParams 2 modes)), 3)
        (7, modes) -> Just ((opFromParams3 LessThan (buildParams 3 modes)), 4)
        (8, modes) -> Just ((opFromParams3 Equals (buildParams 3 modes)), 4)
        (_) -> Nothing
parseOp (_) = Nothing

fetchOp :: Memory -> Param -> Int
fetchOp mem op = case op of
    (Param val ImmediateMode) -> val
    (Param val PosMode) -> mem !! val

fetchOpVal :: Param -> Int
fetchOpVal (Param val _) = val

executeOp :: Program -> Op -> Maybe (Program, Maybe Int)
executeOp (Running mem i o) (Add fst snd ptr) = 
    let res = fetchOp mem fst + fetchOp mem snd
        (beg, end) = splitAt (fetchOpVal ptr) mem
    in Just ((Running (beg ++ [res] ++ tail end) i o), Nothing)
executeOp (Running mem i o) (Mul fst snd ptr) = 
    let res = fetchOp mem fst * fetchOp mem snd
        (beg, end) = splitAt (fetchOpVal ptr) mem
    in Just $ ((Running (beg ++ [res] ++ tail end) i o), Nothing)
executeOp (Running mem i o) (Input ptr) = do
    input <- i !? 0
    Just ((Running (beg ++ [input] ++ tail end) (Seq.drop 1 i) o), Nothing)
    where
        (beg, end) = splitAt ptr mem
executeOp (Running mem i o) (Output fst) =
    Just ((Running mem i $ (fetchOp mem fst) <| o), Nothing)
executeOp (Running mem i o) (JumpIfTrue fst snd) =
    if (fetchOp mem fst /= 0)
        then Just ((Running mem i o), Just $ fetchOp mem snd)
        else Just ((Running mem i o), Nothing)
executeOp (Running mem i o) (JumpIfFalse fst snd) =
    if (fetchOp mem fst == 0)
        then Just ((Running mem i o), Just $ fetchOp mem snd)
        else Just ((Running mem i o), Nothing)
executeOp (Running mem i o) (LessThan fst snd ptr) =
    Just ((Running (beg ++ [val] ++ tail end) i o), Nothing)
    where
        val = if (fetchOp mem fst) < (fetchOp mem snd) then 1 else 0
        (beg, end) = splitAt (fetchOpVal ptr) mem
executeOp (Running mem i o) (Equals fst snd ptr) =
    Just ((Running (beg ++ [val] ++ tail end) i o), Nothing)
    where
        val = if (fetchOp mem fst) == (fetchOp mem snd) then 1 else 0
        (beg, end) = splitAt (fetchOpVal ptr) mem
executeOp (Running mem i o) End = Just ((Halted mem o), Nothing)

rewindProgram :: Program -> Int -> Program
rewindProgram (Running prog i o) ptr = Running (drop ptr prog) i o
rewindProgram (non_running) ptr = non_running

interpret_ :: Int -> RunningProgram
interpret_ ptr = do
    prog <- get
    case prog of
        Halted mem o -> put $ Halted mem o   -- should never happen
        Running mem i o -> 
            case execute_ prog of
                Nothing -> put $ Error mem ptr
                Just (prog, jmp) -> do
                    put prog 
                    interpret_ jmp
        Error mem _ -> put $ Error mem ptr
    where 
        execute_ prog = do
            (op, op_len) <- parseOp (rewindProgram prog ptr) 
            mod_prog <- executeOp prog op
            case mod_prog of
                (p, Nothing) -> return (p, ptr + op_len)
                (p, Just new_ptr) -> return (p, new_ptr)

loadProgram :: IO Memory
loadProgram = liftM ((map read) . splitOn ",") . readFile $ "input.txt"

initAmp :: Memory -> Int -> Int -> Program
initAmp mem phase input = Running mem (Seq.fromList [phase, input]) Seq.empty

runProgram :: Program -> Program
runProgram program = execState (interpret_ 0) program

readOutput :: Program -> Maybe Int
readOutput (Halted mem o) = o !? 0
readOutput (_) = Nothing

type Amp = Int -> Int -> Program

runPhase_ :: Int -> [Amp] -> [Int] -> Maybe Int
runPhase_ input (amp:amps) (ph:phs) = do
    out <- readOutput . runProgram $ (amp ph input)
    runPhase_ out amps phs
runPhase_ input _ [] = Just input

amps :: Memory -> [Amp]
amps mem = take 5 . repeat $ (initAmp mem)

day7 :: IO ()
day7 = do
    mem <- loadProgram
    case liftM maximum $ sequence (thrusts mem) of
        Just max -> print max
        Nothing -> print "Error"
    where
        thrusts mem = map (runPhase_ 0 (amps mem)) $ permutations [0..4]
