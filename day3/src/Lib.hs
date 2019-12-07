module Lib
    ( day3
    ) where

import Control.Monad
import qualified Control.Monad.State as St
import Data.List (sortOn)
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
    , currentCoord :: Coord
}

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
run start (Segment L dst) = zip (repeat $ fst start) [-1, (snd start)..(snd start) - dst -1]
run start (Segment R dst) = zip (repeat $ fst start) [(snd start)..(snd start) + dst - 1]
run start (Segment U dst) = zip [(fst start)..(fst start) + dst - 1] (repeat $ snd start) 
run start (Segment D dst) = zip [-1, (fst start)..(fst start) - dst + 1] (repeat $ snd start) 

runWire :: Wire -> St.State WireRun ()
runWire (segment : rst) = do
    currentRun <- St.get
    let start = currentCoord currentRun
        currentCoords = coords currentRun
        newRun = run start segment
    St.put $ WireRun{
        coords = currentCoords `Set.union` (Set.fromList newRun),
        currentCoord = last newRun
    }
    runWire rst
runWire [] = return ()

emptyRun :: WireRun
emptyRun = WireRun{coords=Set.empty, currentCoord=(0, 0)}

runWires :: [Wire] -> [WireRun]
runWires = map $ (\w -> St.execState (runWire w) emptyRun)

crossingPoints :: [WireRun] -> [Coord]
crossingPoints = Set.toList . foldr Set.intersection Set.empty . (map coords)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

closest :: Coord -> ([Coord] -> Coord)
closest start = head . (sortOn $ manhattan start) . filter (/= start)

day3 :: IO ()
day3 = do
    raw_data <- readData
    case (parse wires "" raw_data) of
        Left err -> print err
        Right ws -> do
            let getClosest = closest (0, 0) . crossingPoints . runWires 
            print (manhattan (0, 0) $ getClosest ws)
