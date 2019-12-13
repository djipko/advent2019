module Lib
    ( day8
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

day8 :: IO ()
day8 =
    loadImage >>= print . (\l -> (count '2' l) * (count '1' l)) . layer
    where 
        layer = fst . lowest '0' . splitLayers
