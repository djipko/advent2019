module Lib
    ( day2
    , runProgram
    , Program(..)
    ) where

import Control.Monad
import Control.Monad.State
import Data.List.Split
import Debug.Trace

data Program = Running [Int] | Halted [Int] | Error Int deriving Show

type RunningProgram = State Program ()

data Op = Add Int Int Int | Mul Int Int Int | End deriving Show

executeOp :: Program -> Op -> Maybe Program
executeOp (Running prog) (Add fst snd ptr) = 
    let res = prog !! fst + prog !! snd
        (beg, end) = splitAt ptr prog
    in Just $ Running $ beg ++ [res] ++ tail end
executeOp (Running prog) (Mul fst snd ptr) = 
    let res = prog !! fst * prog !! snd
        (beg, end) = splitAt ptr prog
    in Just $ Running $ beg ++ [res] ++ tail end
executeOp (Running prog) End = Just (Halted prog)

parseOp :: Program -> Maybe Op
parseOp (Running (99 : _)) = Just End
parseOp (Running (1 : fst : snd : res : _)) = Just (Add fst snd res)
parseOp (Running (2 : fst : snd : res : _)) = Just (Mul fst snd res)
parseOp (_) = Nothing

rewindProgram :: Program -> Int -> Program
rewindProgram (Running prog) pos = Running $ drop pos prog
rewindProgram (non_running) pos = non_running

interpret_ :: Int -> RunningProgram
interpret_ pos = do
    let execute_ prog = parseOp (rewindProgram prog pos) >>= executeOp prog
    prog <- get
    traceM ("pos: " ++ show pos ++ "; prog: " ++ show prog)
    case prog of
        Halted cur_prog -> put $ Halted cur_prog   -- should never happen
        Running cur_prog -> 
            case execute_ prog of
                Nothing -> put $ Error pos
                Just prog -> do
                    put prog 
                    interpret_ (pos + 4)
        Error _ -> put $ Error pos

getProgram :: IO Program
getProgram = liftM (Running . (map read) . (splitOn ",")) . readFile $ "input.txt" 

runProgram :: Program -> IO Program
runProgram program = return $ execState (interpret_ 0) program

patch :: Program -> Program
patch (Running (fst : pos1 : pos2 : rest)) = Running $ fst : 12 : 2 : rest
patch (_) = Error 0

day2 :: IO ()
day2 = do
    borked <- getProgram
    fixed <- return $ patch borked
    putStrLn . show $ fixed
    final <- runProgram fixed
    case final of
        Halted prog -> putStrLn . show $ prog !! 0
        _ -> putStrLn ("Error executing program: " ++ show final)
