module Day17 (day17) where

import Control.Monad.RWS (RWS, execRWS, get, put, tell)
import Data.Char (digitToInt)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Heap as H

type Grid = V.Vector (V.Vector Int)
type Node = (Int, Int)
data Direction = N | S | E | W deriving (Eq, Show, Ord)
data TraveledNode = TraveledNode { 
    node :: Node, 
    traveled :: Int,
    dir :: Direction
} deriving (Show, Eq, Ord)

type State = (S.Set TraveledNode, H.MinPrioHeap Int TraveledNode)

parseGrid :: [String] -> Grid
parseGrid = V.fromList . fmap (V.fromList . fmap digitToInt)

initialState :: State
initialState = (S.empty, H.empty)

initialNode :: TraveledNode
initialNode =  TraveledNode (0,0) 0 E

getNeigbors :: Int -> Int -> TraveledNode -> [TraveledNode]
getNeigbors lenX lenY (TraveledNode (x,y) t d) = concatMap ns ds where
    ds = if t == 3 then filter (/=d) (dirs d) else dirs d
    ns newD = fmap (\(a,b) -> (TraveledNode (a,b) (if newD == d then t+1 else 1) newD)) (safeNeighbor lenX lenY (x,y) newD 1)

getNeigborsPart2 :: Int -> Int -> TraveledNode -> [TraveledNode]
getNeigborsPart2 lenX lenY (TraveledNode (x,y) t d) = concatMap ns ds where
    ds = if t == 10 then filter (/=d) (dirs d) else dirs d
    ns newD = fmap (\(a,b) -> (TraveledNode (a,b) (if newD == d && ((x,y)/=(0,0)) then t+1 else 4) newD)) (safeNeighbor lenX lenY (x,y) newD (if newD == d && ((x,y)/=(0,0)) then 1 else 4))

dirs :: Direction -> [Direction]
dirs E = [E, N, S]
dirs W = [W, N, S]
dirs N = [N, E, W]
dirs S = [S, E, W]

safeNeighbor :: Int -> Int -> (Int, Int) -> Direction -> Int -> [(Int, Int)]
safeNeighbor lenX lenY (x,y) d n
  | d == E && x < lenX-n = [(x+n,y)]
  | d == W && x > n-1 = [(x-n,y)]
  | d == N && y > n-1 = [(x,y-n)]
  | d == S && y < lenY-n = [(x,y+n)]
  | otherwise = []

getWeight :: Grid -> TraveledNode -> Int
getWeight g (TraveledNode (x,y) _ _) = g V.! y V.! x

getWeightPart2 :: Grid -> TraveledNode -> Int
getWeightPart2 g (TraveledNode (x,y) t d) = if t == 4 then cur + prev else cur where
    cur = g V.! y V.! x
    prev = case (d) of 
        E -> (g V.! y V.! (x-1)) + (g V.! y V.! (x-2)) + (g V.! y V.! (x-3))
        W -> (g V.! y V.! (x+1)) + (g V.! y V.! (x+2)) + (g V.! y V.! (x+3))
        N -> (g V.! (y+1) V.! x) + (g V.! (y+2) V.! x) + (g V.! (y+3) V.! x)
        S -> (g V.! (y-1) V.! x) + (g V.! (y-2) V.! x) + (g V.! (y-3) V.! x)

dijkstra :: Bool -> Grid -> Int
dijkstra part1 g = minimum . snd $ execRWS (inner 0 initialNode) () initialState where
    inner :: Int -> TraveledNode -> RWS () [Int] State ()
    inner cost n = do
        (visited, weights) <- get
        let newVisited = S.insert n visited
        let next = filter (\a -> S.notMember a newVisited) $ if part1 then (getNeigbors lenX lenY n) else (getNeigborsPart2 lenX lenY n)  
        let nextWithCost = fmap (\a -> (a, cost + (if part1 then getWeight g a else getWeightPart2 g a))) next
        let newWeights = foldr (\(k,v) r -> H.insert (v,k) r) weights nextWithCost
        let minCost = H.view newWeights
        if (node n) == (lenX-1,lenY-1) then tell [cost] else (traverse (\((a,b),h) -> put (newVisited, h) >> inner a b) minCost) >> return ()
    lenY = V.length g
    lenX = V.length . V.head $ g


day17 :: String -> (Int, Int)
day17 input = (part1, part2) where
    grid = parseGrid (lines input)
    part1 = dijkstra True grid
    part2 = dijkstra False grid