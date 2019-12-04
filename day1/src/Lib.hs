module Lib
    ( day1
    , day1_1
    
    ) where

import Control.Monad
import System.IO

getModules :: IO [Integer]
getModules = liftM (map (\mass -> read mass ) . lines) . readFile $ "input.txt" 

day1 :: IO ()
day1 = do
    file_data <- getModules
    let fuel = [f `div` 3 - 2 | f <- file_data]
    (putStrLn . show) (foldr (+) 0 fuel)

fuel :: Integer -> Integer
fuel mass = mass `div` 3 - 2

massWFuel :: Integer -> Integer
massWFuel mass = 
    let fm = fuel mass
    in if fm <= 0 then 0
       else fm + massWFuel fm

day1_1 :: IO ()
day1_1 = do
    file_data <- getModules
    let mwf =[massWFuel m | m <- file_data]
    (putStrLn . show) (foldr (+) 0 mwf)
