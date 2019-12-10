module Lib
    ( day6
    , day6_1
    ) where

import Control.Monad
import Data.Foldable (toList)
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Tree

type Graph = Map.Map String (Seq String)

loadFile :: String -> IO [String]
loadFile = liftM (splitOn "\n") . readFile

loadGraph :: IO Graph
loadGraph = liftM (buildMap . (map $ splitOn ")")) $ loadFile "input.txt"
    where
        alter_ v Nothing = Just $ Seq.singleton v
        alter_ v (Just s) = Just $ s |> v
        addToMap (k : v : []) = Map.alter (alter_ v) k
        addToMap (_) = id
        buildMap = foldr addToMap Map.empty

nodeBuilder :: Graph -> String -> (String, [String])
nodeBuilder m v =
    case m Map.!? v of
        Nothing -> (v, [])
        Just s -> (v, toList s)

day6 :: IO ()
day6 = do
    g <- loadGraph
    let l = levels $ unfoldTree (nodeBuilder g) "COM"
    let f (d, l) s = s + d * length l
    print $ foldr f 0 $ zip [0..] l

foldPath :: String -> String -> [Seq String] -> Seq String
foldPath el x xs
    | el == x = Seq.singleton x
    | otherwise = case sortOn length $ map ((<|) x) $ filter (not . null) xs of
        [] -> Seq.Empty
        (s:_) -> s

commonPrefix :: Seq String -> Seq String -> Int
commonPrefix = curry $ Seq.length . Seq.takeWhileL (\(x, y) -> x == y) . (uncurry Seq.zip)

day6_1 :: IO ()
day6_1 = do
    g <- loadGraph
    let tree = unfoldTree (nodeBuilder g) "COM"
    let you_path = foldTree (foldPath "YOU") tree
    let san_path = foldTree (foldPath "SAN") tree
    let common = commonPrefix san_path you_path
    print $ ((length you_path) - common) + ((length san_path) - common) - 2
