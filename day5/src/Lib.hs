module Lib
    ( day5
    , runProgram
    , Program(..)
    , parseOpcode
    , parseOp
    ) where

import Control.Monad
import Control.Monad.State
import Data.List.Split
import Data.Sequence (Seq, (<|), (!?))
import qualified Data.Sequence as Sq

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
    | End deriving Show

threeParamOps = ['1', '2']

oneParamOps = ['4']

parseExtended digit modes
        | digit `elem` threeParamOps =
            -- see Traversable instance of (,) a to see why this works
            sequence ((read [digit]), (parseModes_ 3 modes))
        | digit `elem` oneParamOps =
            -- see Traversable instance of (,) a to see why this works
            sequence ((read [digit]), (parseModes_ 1 modes))
        | otherwise = Nothing

parseBase digit
        | digit `elem` threeParamOps =
            return ((read [digit]) , replicate 3 PosMode)
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

opFromParams :: (Param -> Param -> Param -> Op) -> [Param] -> Op
opFromParams opC params = opC (params !! 0) (params !! 1) (params !! 2)

parseOp :: Program -> Maybe (Op, Int)
parseOp (Running (99 : _) _ _) = Just (End, 0)
parseOp (Running (3 : ptr : _) _ _) = Just (Input ptr, 2)
parseOp (Running (opcode : rst) _ _) = do
    let threeParams = map (uncurry Param) . zip (take 3 rst)
    let oneParam modes = Param (head rst) (head modes)
    parsed <- parseOpcode opcode
    case parsed of
        (1, modes) -> Just ((opFromParams Add (threeParams modes)), 4)
        (2, modes) -> Just ((opFromParams Mul (threeParams modes)), 4)
        (4, modes) -> Just ((Output $ oneParam modes), 2)
        (_) -> Nothing
parseOp (_) = Nothing

fetchOp :: Memory -> Param -> Int
fetchOp mem op = case op of
    (Param val ImmediateMode) -> val
    (Param val PosMode) -> mem !! val

fetchOpVal :: Param -> Int
fetchOpVal (Param val _) = val

executeOp :: Program -> Op -> Maybe Program
executeOp (Running mem i o) (Add fst snd ptr) = 
    let res = fetchOp mem fst + fetchOp mem snd
        (beg, end) = splitAt (fetchOpVal ptr) mem
    in Just $ Running (beg ++ [res] ++ tail end) i o
executeOp (Running mem i o) (Mul fst snd ptr) = 
    let res = fetchOp mem fst * fetchOp mem snd
        (beg, end) = splitAt (fetchOpVal ptr) mem
    in Just $ Running (beg ++ [res] ++ tail end) i o
executeOp (Running mem i o) (Input ptr) = do
    input <- i !? 0
    Just $ Running (beg ++ [input] ++ tail end) (Sq.drop 1 i) o
    where
        (beg, end) = splitAt ptr mem
executeOp (Running mem i o) (Output fst) = Just $ Running mem i $ (fetchOp mem fst) <| o
executeOp (Running mem i o) End = Just (Halted mem o)

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
                    interpret_ (ptr + jmp)
        Error mem _ -> put $ Error mem ptr
    where 
        execute_ prog = do
            (op, jmp) <- parseOp (rewindProgram prog ptr) 
            mod_prog <- executeOp prog op
            return (mod_prog, jmp)

loadProgram :: Seq Int -> IO Program
loadProgram input = do
    init_mem <- liftM ((map read) . splitOn ",") . readFile $ "input.txt"
    return $ Running init_mem input (Sq.empty)

runProgram :: Program -> IO Program
runProgram program = return $ execState (interpret_ 0) program

validateResult :: Program -> IO ()
validateResult (Halted mem o) = print o
validateResult (error) = print error

day5 :: IO ()
day5 = loadProgram (Sq.singleton 1) >>= runProgram >>= validateResult
