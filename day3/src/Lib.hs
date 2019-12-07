module Lib
    ( day3
    , run
    , Direction(..)
    , Segment(..)
    , manhattan
    ) where

import Control.Monad
import qualified Control.Monad.State as St
import Data.List (sortOn, elemIndices)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Char

data Direction = L | R | U | D deriving Show

data Segment = Segment Direction Int deriving Show

type Wire = [Segment]

type Coord = (Int, Int)

data WireRun = WireRun {
      coords :: Set Coord
    , orderedCoords :: [Coord]
    , currentCoord :: Coord
} deriving Show

direction :: Parsec String s Direction
direction = do
    ch <- oneOf "LRUD"
    case ch of
        'L' -> return L
        'R' -> return R
        'U' -> return U
        'D' -> return D

distance :: Parsec String s Int
distance = liftM read $ many digit

segment :: Parsec String s Segment
segment = do
    dir <- direction
    dist <- distance
    return $ Segment dir dist

wire :: Parsec String s Wire
wire = segment `sepBy` (char ',')

wires :: Parsec String s [Wire]
wires = wire `endBy` (char '\n')

readData :: IO String
readData = readFile "input.txt"

run :: Coord -> Segment -> [Coord]
run start (Segment L dst) =
    let range_start = (fst start) - 1
        range_end = range_start - dst + 1
    in zip [range_start, (range_start-1)..range_end] (repeat $ snd start) 
run start (Segment R dst) =
    let range_start = (fst start) + 1
        range_end = range_start + dst - 1
    in zip [range_start..range_end] (repeat $ snd start) 
run start (Segment U dst) =
    let range_start = (snd start) + 1
        range_end = range_start + dst - 1
    in zip (repeat $ fst start) [range_start..range_end]  
run start (Segment D dst) =
    let range_start = (snd start) - 1
        range_end = range_start - dst + 1
    in zip (repeat $ fst start) [range_start, (range_start - 1)..range_end]  

runWire :: Wire -> St.State WireRun ()
runWire (segment : rst) = do
    currentRun <- St.get
    let start = currentCoord currentRun
        currentCoords = coords currentRun
        newRun = run start segment
    St.put $ WireRun{
        coords = currentCoords `Set.union` (Set.fromList newRun),
        orderedCoords = (orderedCoords currentRun) ++ newRun,
        currentCoord = last newRun
    }
    runWire rst
runWire [] = return ()

emptyRun :: WireRun
emptyRun = WireRun{
    coords=Set.singleton (0, 0),
    orderedCoords=[(0, 0)],
    currentCoord=(0, 0)
}

runWires :: [Wire] -> [WireRun]
runWires = map $ (\w -> St.execState (runWire w) emptyRun)

-- Currently deals only with first two
crossingPoints :: [WireRun] -> [Coord]
crossingPoints (w1 : w2 : _) = Set.toList $ (coords w1) `Set.intersection` (coords w2)
crossingPoints (_) = []

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

closest :: Coord -> ([Coord] -> Coord)
closest start = head . (sortOn $ manhattan start) . filter (/= start)

totalLatency :: Coord -> [WireRun] -> Int
totalLatency cross = (foldr (+) 0) . (map $ head . elemIndices cross . orderedCoords)

day3 :: IO ()
day3 = do
    raw_data <- readData
    case (parse wires "" raw_data) of
        Left err -> print err
        Right ws -> do
            let wr = runWires $ ws
            let getClosest = closest (0, 0) . crossingPoints . runWires 
            let cs = filter (/= (0, 0)) $ crossingPoints wr
            let latencies = map (flip totalLatency $ wr) cs
            print (manhattan (0, 0) $ getClosest ws)
            print (minimum latencies)
