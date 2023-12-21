module Day21 (test) where

import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Vector as V

data Plot = Rock | Empty | Start deriving (Eq, Show)
type Coord = (Int, Int)
type Garden = V.Vector (V.Vector (Plot))

parseGarden :: [String] -> Garden
parseGarden = V.fromList . fmap (V.fromList . fmap parsePlot) where
    parsePlot '#' = Rock
    parsePlot 'S' = Start
    parsePlot _ = Empty

lookupGarden :: Coord -> Garden -> Plot
lookupGarden (x,y) g = g V.! y V.! x

findAdjacent :: Garden -> Coord -> [Coord]
findAdjacent g (x,y) = filter safe allAdj where
    allAdj = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
    safe (a,b) = a < lenX && b < lenY && ((lookupGarden (a,b) g) == Empty || (lookupGarden (a,b) g) == Start)
    lenX = V.length . V.head $ g
    lenY = V.length g

findStart :: Garden -> Coord
findStart g = findRow where
    findCol s = foldr (\(i,c) r -> if c == Start then i else r) 0 (zip [0..] (V.toList s))
    findRow = foldr (\(j,s) r -> let i = (findCol s) in if i /= 0 then (i,j) else r) (0,0) (zip [0..] (V.toList g))

bfs :: Int -> Garden -> Int
bfs n g = last . snd $ evalRWS (inner 0 [findStart g]) () S.empty where
    inner :: Int -> [Coord] -> RWS () [Int] (S.Set Coord) ()
    inner c cur
      | c == n = return ()
      | otherwise = do
        let next = nub . concatMap (findAdjacent g) $ cur
        tell [length next]
        inner (c+1) next

test :: IO ()
test = do
    input <- readFile "data/day21.txt"
    let garden = parseGarden (lines input)
    let part1 = bfs 64 garden 
    print part1