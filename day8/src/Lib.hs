module Lib
    ( day8
    , day8_1
    , loadImage
    , splitLayers
    , mergeLayers
    ) where

import Data.List
import Data.List.Split

loadImage :: IO String
loadImage = readFile "input.txt"

width = 25
height = 6

layerLen = width * height

splitLayers = divvy layerLen layerLen

count ch = length . filter (== ch)

lowest ch layers = head $ sortOn snd $ zip layers $ map (count ch) layers

mergeLayers :: [String] -> String
mergeLayers layers = 
    (head $ (dropWhile (== '2')) pixels) : doRest
    where 
        rest = map tail layers
        doRest = if null . sequence $ rest
            then []
            else mergeLayers rest
        pixels = map head layers

day8 :: IO ()
day8 =
    loadImage >>= print . (\l -> (count '2' l) * (count '1' l)) . layer
    where 
        layer = fst . lowest '0' . splitLayers

day8_1 = do
    loadImage >>= mapM_ putStrLn . (divvy width width) . map replace . mergeLayers . splitLayers
    where
        replace c = if c /= '0' then c else ' '
