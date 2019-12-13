module Lib
    ( day7
    , day7_1
    , runProgram
    , Program(..)
    , parseOpcode
    , parseOp
    , generateOutput
    , getLastOutput
    , step
    , passInput
    , ampsFb
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

data Program = Running Memory Input Output Pc
    | Halted Memory Output
    | Error Memory Pc deriving Show

type RunningProgram = State Program ()

type OutProgram = State Program (Maybe Int)

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
parseOp (Running (99 : _) _ _ _) = Just (End, 0)
parseOp (Running (3 : ptr : _) _ _ _) = Just (Input ptr, 2)
parseOp (Running (opcode : rst) _ _ _) = do
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
executeOp (Running mem i o pc) (Add fst snd ptr) = 
    let res = fetchOp mem fst + fetchOp mem snd
        (beg, end) = splitAt (fetchOpVal ptr) mem
    in Just ((Running (beg ++ [res] ++ tail end) i o pc), Nothing)
executeOp (Running mem i o pc) (Mul fst snd ptr) = 
    let res = fetchOp mem fst * fetchOp mem snd
        (beg, end) = splitAt (fetchOpVal ptr) mem
    in Just $ ((Running (beg ++ [res] ++ tail end) i o pc), Nothing)
executeOp (Running mem i o pc) (Input ptr) = do
    input <- i !? 0
    Just ((Running (beg ++ [input] ++ tail end) (Seq.drop 1 i) o pc), Nothing)
    where
        (beg, end) = splitAt ptr mem
executeOp (Running mem i o pc) (Output fst) =
    Just ((Running mem i ((fetchOp mem fst) <| o) pc), Nothing)
executeOp (Running mem i o pc) (JumpIfTrue fst snd) =
    if (fetchOp mem fst /= 0)
        then Just ((Running mem i o pc), Just $ fetchOp mem snd)
        else Just ((Running mem i o pc), Nothing)
executeOp (Running mem i o pc) (JumpIfFalse fst snd) =
    if (fetchOp mem fst == 0)
        then Just ((Running mem i o pc), Just $ fetchOp mem snd)
        else Just ((Running mem i o pc), Nothing)
executeOp (Running mem i o pc) (LessThan fst snd ptr) =
    Just ((Running (beg ++ [val] ++ tail end) i o pc), Nothing)
    where
        val = if (fetchOp mem fst) < (fetchOp mem snd) then 1 else 0
        (beg, end) = splitAt (fetchOpVal ptr) mem
executeOp (Running mem i o pc) (Equals fst snd ptr) =
    Just ((Running (beg ++ [val] ++ tail end) i o pc), Nothing)
    where
        val = if (fetchOp mem fst) == (fetchOp mem snd) then 1 else 0
        (beg, end) = splitAt (fetchOpVal ptr) mem
executeOp (Running mem i o pc) End = Just ((Halted mem o), Nothing)

rewindProgram :: Program -> Program
rewindProgram (Running prog i o pc) = Running (drop pc prog) i o 0
rewindProgram (non_running) = non_running

movePc :: Program -> Int -> Program
movePc (Running prog i o pc) new_pc = Running prog i o new_pc
movePc (non_running) ptr = non_running

getPc :: Program -> Maybe Int
getPc (Running prog i o pc) = Just pc
getPc (non_running) = Nothing

step :: RunningProgram
step = do
    prog <- get
    case prog of
        Halted mem o -> put $ Halted mem o
        Running mem i o pc -> 
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

initAmp :: Memory -> Int -> Int -> Program
initAmp mem phase input = Running mem (Seq.fromList [phase, input]) Seq.empty 0

initAmpFb :: Memory -> Int -> Program
initAmpFb mem phase = Running mem (Seq.fromList [phase]) Seq.empty 0

initAmpA :: Memory -> Int -> Program
initAmpA mem phase = Running mem (Seq.fromList [phase, 0]) Seq.empty 0

runProgram :: Program -> Program
runProgram program = execState interpret program

passInput :: Int -> Program -> Program
passInput val (Running m i o pc) = Running m (i |> val) o pc
passInput val prog = prog

readOutput :: Program -> Maybe Int
readOutput (Halted mem o) = o !? 0
readOutput (_) = Nothing

getLastOutput :: Program -> Maybe Int
getLastOutput (Running mem i o pc) = o !? 0
getLastOutput (Halted mem o) = o !? 0
getLastOutput (_) = Nothing

dropOutput :: Program -> Program
dropOutput (Running mem i o pc) = Running mem i Seq.empty pc
dropOutput prog = prog

type Amp = Int -> Int -> Program
type AmpFb = Program

runPhase_ :: Int -> [Amp] -> [Int] -> Maybe Int
runPhase_ input (amp:amps) (ph:phs) = do
    out <- readOutput . runProgram $ (amp ph input)
    runPhase_ out amps phs
runPhase_ input _ [] = Just input

amps :: Memory -> [Amp]
amps mem = take 5 . repeat $ (initAmp mem)

ampsFb :: Memory -> [Int] -> [AmpFb]
ampsFb mem (p:ps) = (execState (generateOutput 0) (initAmpFb mem p)) : map (initAmpFb mem) ps

generateOutput :: Int -> OutProgram
generateOutput input = do
    modify' (passInput input)
    runUntil output_
    prog <- get
    case prog of
        Running m i o pc -> return $ o !? 0
        Halted m o -> return $ o !? 0
    where
        output_ (Running m i o pc) = not . null $ o
        output_ _ = True

runPhaseFb :: [AmpFb] -> Maybe Int
runPhaseFb (fst : snd : rst) = 
    case getLastOutput fst of
        Nothing -> Nothing
        Just val ->
            case (out, snd') of
                (_, (Running m i o pc)) -> runPhaseFb ([snd'] ++ rst ++ [fst])
                (_, (Halted m o)) -> Just val
            where
                (out, snd') = runState (generateOutput val) (dropOutput snd)

day7 :: IO ()
day7 = do
    mem <- loadProgram
    case liftM maximum $ sequence (thrusts mem) of
        Just max -> print max
        Nothing -> print "Error"
    where
        thrusts mem = map (runPhase_ 0 (amps mem)) $ permutations [0..4]

day7_1 :: IO ()
day7_1 = do
    mem <- loadProgram
    case liftM maximum $ sequence (thrusts mem) of
        Just max -> print max
        Nothing -> print "Error"
    where
        thrusts mem = map runPhaseFb $ map (ampsFb mem) $ permutations [5..9]
