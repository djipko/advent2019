module Lib
    ( day2
    , day2_1
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

patch :: Int -> Int -> Program -> Program
patch ptr val (Running prog) = Running $ take ptr prog ++ [val] ++ (tail . drop ptr) prog 
patch ptr val prog = prog

day2 :: IO ()
day2 = do
    borked <- getProgram
    fixed <- return $ (patch 1 12 . patch 2 2) borked
    putStrLn . show $ fixed
    final <- runProgram fixed
    case final of
        Halted prog -> putStrLn . show $ prog !! 0
        _ -> putStrLn ("Error executing program: " ++ show final)

patch_ :: Int -> Int -> (Program -> Program)
patch_ noun verb = (patch 1 noun . patch 2 verb)

check_ :: Program -> (Int, Int) -> IO ()
check_ program (noun, verb) = do
    let solution noun verb = 100 * noun + verb
    patched <- return (patch_ noun verb $ program)
    final <- runProgram patched
    case final of
        Halted prog ->
            if prog !! 0 == 19690720
                then do 
                    putStrLn (show noun ++ " " ++ show verb)
                    putStrLn ("-> " ++ (show $ solution noun verb))
                else return ()
        _ -> return ()

day2_1 = do
    borked <- getProgram
    mapM_ (check_ borked) [(verb, noun) | verb <- [0..99], noun <- [0..99]]
