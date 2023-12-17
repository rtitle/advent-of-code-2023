module Day16 (day16) where

import Control.Monad.RWS (RWS, evalRWS, get, put, tell)
import qualified Data.Set as S
import qualified Data.Vector as V

data Tile = Empty | FMirror | BMirror | HSplit | VSplit deriving (Eq, Show)
type Grid = V.Vector (V.Vector Tile)
type Pos = (Int, Int)
data Direction = N | S | E | W deriving (Eq, Show, Ord)
type Beam = (Pos, Direction)

parseGrid :: [String] -> Grid
parseGrid = V.fromList . fmap (V.fromList . fmap parseTile) where
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
    lenY = V.length g
    lenX = V.length . V.head $ g

simulate :: Grid -> [Beam] -> Int
simulate g = maximum . fmap (\i -> S.size . snd $ evalRWS (inner [i]) () S.empty) where
    inner :: [Beam] -> RWS () (S.Set Pos) (S.Set Beam) ()
    inner [] = return ()
    inner bs = do
        tell (S.fromList . fmap fst $ bs)
        cache <- get
        let next = filter (`S.notMember` cache) . concat . fmap step $ bs
        put (foldr (S.insert) cache next)
        inner next
    step b@((x,y), d) = case (g V.! y V.! x) of
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
    lenX = length . V.head $ g
    lenY = length g

day16 :: String -> (Int, Int)
day16 input = (part1, part2) where
    grid = parseGrid (lines input)
    part1 = simulate grid [initial]
    part2 = simulate grid (initials grid)