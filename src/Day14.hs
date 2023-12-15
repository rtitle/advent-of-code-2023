module Day14 (day14) where

import Data.List (sortBy, tails)
import Data.Ord (comparing)
import qualified Data.Map as M

data Cell = O | R | Empty deriving (Eq, Show)
type Coord = (Int, Int)
data Grid = Grid {
    m :: M.Map Coord Cell,
    lenX :: Int,
    lenY :: Int
} deriving (Eq, Show)
data Direction = N | S | E | W deriving (Eq, Show)

parseGrid :: [String] -> Grid
parseGrid ls = Grid m' lenX' lenY' where
    m' = M.fromList . concat . fmap parseLine . zip [0..] $ ls
    parseLine (y, s) = fmap (\(x, a) -> ((x, y), parseCell a)) (zip [0..] s)
    parseCell 'O' = O
    parseCell '#' = R
    parseCell _ = Empty
    lenY' = length ls
    lenX' = length . head $ ls

rollAll :: Direction -> Grid -> Grid
rollAll d g = foldl (roll d) g s where
    cs = fmap fst . filter ((==O) . snd) . M.toList . m $ g
    s = case d of
        N -> sortBy (comparing snd) cs
        S -> reverse . sortBy (comparing snd) $ cs
        W -> sortBy (comparing fst) cs
        E -> reverse . sortBy (comparing fst) $ cs

roll :: Direction -> Grid -> Coord -> Grid
roll d g (x,y)
  | stay d g (x,y) = g
  | otherwise = roll d newGrid newCoord where
    newCoord = rollCoord d (x,y)
    newGrid = Grid (M.insert newCoord O (M.insert (x,y) Empty (m g))) (lenX g) (lenY g)

stay :: Direction -> Grid -> Coord -> Bool
stay N g (x,y) = y == 0 || wall ((m g) M.! rollCoord N (x,y))
stay S g (x,y) = y == (lenY g) - 1 || wall ((m g) M.! rollCoord S (x,y))
stay W g (x,y) = x == 0 || wall ((m g) M.! rollCoord W (x,y))
stay E g (x,y) = x == (lenX g) - 1 || wall ((m g) M.! rollCoord E (x,y))

totalLoad :: Grid -> Int
totalLoad g = sum . fmap inner . M.toList $ (m g) where
    inner ((_,y), O) = (lenY g) - y
    inner _ = 0

wall :: Cell -> Bool
wall O = True
wall R = True
wall _ = False

rollCoord :: Direction -> Coord -> Coord
rollCoord N (x,y) = (x,y-1)
rollCoord S (x,y) = (x,y+1)
rollCoord E (x,y) = (x+1,y)
rollCoord W (x,y) = (x-1,y)

rollCycle :: Grid -> Grid
rollCycle g = rollAll E (rollAll S (rollAll W (rollAll N g)))

getPeriodic :: [Int] -> [Int]
getPeriodic is = maximum (inner 1) where
    inner n
      | n * 2 >= length is = []
      | take n is == take n (drop n is) = [take n is] ++ inner (n+1)
      | otherwise = inner (n+1)

day14 :: String -> (Int, Int)
day14 input = (part1, part2) where
    ls = lines input
    grid = parseGrid ls
    rolled = rollAll N grid
    part1 = totalLoad rolled
    loads = fmap totalLoad $ foldr (\_ r -> (rollCycle (head r)) : r) [grid] (take 200 (repeat 'a'))
    period = getPeriodic loads
    periodLen = length period
    run = fst . head . filter (\(_, t) -> take periodLen t == (reverse period)) $ zip [0..] (tails (reverse loads))
    remainder = (1000000000 - run + 1) `mod` periodLen
    part2 = head . reverse . take remainder . reverse $ period