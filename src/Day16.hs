module Day16 (test) where

import Control.Monad.RWS (RWS, evalRWS, execRWS, put, tell)
import Data.List (nub)
import qualified Data.Set as S

data Tile = Empty | FMirror | BMirror | HSplit | VSplit deriving (Eq, Show)

type Grid = [[Tile]]

type Pos = (Int, Int)

data Direction = N | S | E | W deriving (Eq, Show, Ord)

type Beam = (Pos, Direction)

type Cache = S.Set Beam

parseGrid :: [String] -> Grid
parseGrid = fmap (fmap parseTile) where
    parseTile '/' = FMirror
    parseTile '\\' = BMirror
    parseTile '-' = HSplit
    parseTile '|' = VSplit
    parseTile _ = Empty

initial :: Beam
initial = ((0,0), E)

initials :: Grid -> [Beam]
initials g = v ++ h where
    v = concatMap (\y -> [((0,y), E), ((lenX-1,y), W)]) [0..lenY-1]
    h = concatMap (\x -> [((x,0), S), ((x,lenY-1), N)]) [0..lenX-1]
    lenY = length g
    lenX = length . head $ g

simulate :: Grid -> [Beam] -> Int
simulate g is = maximum . fmap (\i -> length . nub . snd $ evalRWS (inner [i] S.empty) () ()) $ is where
    inner :: [Beam] -> Cache -> RWS () [Pos] () ()
    inner [] _ = return ()
    inner bs cache = do
        tell (fmap fst bs)
        let next = filter (`S.notMember` cache) . concat . fmap step $ bs
        let newCache = foldr (S.insert) cache next
        inner next newCache
    step b@((x,y), d) = case (g !! y !! x) of
        Empty -> zip (move b) [d]
        FMirror -> let newD = fmirror d in zip (move ((x,y), newD)) [newD]
        BMirror -> let newD = bmirror d in zip (move ((x,y), newD)) [newD]
        HSplit -> if d == E || d == W then zip (move b) [d] else (zip (move ((x,y), W)) [W]) ++ (zip (move ((x,y), E)) [E])
        VSplit -> if d == N || d == S then zip (move b) [d] else (zip (move ((x,y), N)) [N]) ++ (zip (move ((x,y), S)) [S])
    move ((x,y), E) = if x == lenX-1 then [] else [(x+1,y)]
    move ((x,y), W) = if x == 0 then [] else [(x-1,y)]
    move ((x,y), N) = if y == 0 then [] else [(x,y-1)]
    move ((x,y), S) = if y == lenY-1 then [] else [(x,y+1)]
    fmirror N = E
    fmirror S = W 
    fmirror E = N 
    fmirror W = S
    bmirror N = W 
    bmirror S = E 
    bmirror E = S
    bmirror W = N
    lenX = length . head $ g
    lenY = length g

test :: IO ()
test = do
    input <- readFile "data/day16.txt"
    let grid = parseGrid (lines input)
    let part1 = simulate grid [initial]
    let part2 = simulate grid (initials grid)
    print part1
    print part2