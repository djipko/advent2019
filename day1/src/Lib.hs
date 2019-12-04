module Lib
    ( day1
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
