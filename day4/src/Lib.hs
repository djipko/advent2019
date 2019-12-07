module Lib
    ( day4
    ) where

import Data.List

start = 136760
end = 595730

consecutive_ :: String -> Bool
consecutive_ (f : n : rst) = if f == n then True else consecutive_ $ n : rst
consecutive_ _ = False

consecutive :: Int -> Bool
consecutive = consecutive_ . show

ascending_ :: String -> Bool
ascending_ s = (sort s) == s

ascending :: Int -> Bool
ascending = ascending_ . show

day4 :: IO ()
day4 = print $ length . filter (\pw -> consecutive pw && ascending pw) $ [start..end]
