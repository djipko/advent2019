module Lib
    ( day4
    , day4_1
    , hasRun
    , consecutiveRun
    ) where

import Data.List

start = 136760
end = 595730

consecutive_ :: String -> Bool
consecutive_ (f : n : rst) = if f == n then True else consecutive_ $ n : rst
consecutive_ _ = False

consecutiveRun :: Int -> Char -> String -> Int
consecutiveRun run c (x:xs) = if x /= c then run else consecutiveRun (run + 1) c xs
consecutiveRun run c [] = run

hasRun :: Int -> String -> Bool
hasRun run (x:xs) =
    let foundRun = consecutiveRun 1 x xs
    in if foundRun == run then True else hasRun run (drop (foundRun - 1) xs)
hasRun run [] = False

consecutive :: Int -> Bool
consecutive = consecutive_ . show

ascending_ :: String -> Bool
ascending_ s = (sort s) == s

ascending :: Int -> Bool
ascending = ascending_ . show

day4 :: IO ()
day4 = print $ length . filter (\pw -> consecutive pw && ascending pw) $ [start..end]

day4_1 :: IO ()
day4_1 = print $ length . filter (\pw -> hasRun 2 (show pw) && ascending pw) $ [start..end]
