module Day21 (test) where

import Control.Monad.RWS (RWS, execRWS, evalRWS, tell, get, put)
import Data.Function (on)
import Data.List (nub, group, sortBy, groupBy)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M

data Plot = Rock | Empty | Start deriving (Eq, Show)
type Coord = (Int, Int)
type Coord2 = (Coord, Coord)
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
    safe (a,b) = a < lenX && b < lenY && a >= 0 && b >= 0 && ((lookupGarden (a,b) g) == Empty || (lookupGarden (a,b) g) == Start)
    lenX = V.length . V.head $ g
    lenY = V.length g

findAdjacent2 :: Garden -> Coord2 -> [Coord2]
findAdjacent2 g ((x,y),(gx,gy)) = filter safe . fmap expand $ allAdj where
    allAdj = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
    safe ((a,b),_) = (lookupGarden (a,b) g) == Empty || (lookupGarden (a,b) g) == Start
    expand (a,b)
      | a < 0 = ((lenX-1,y),(gx-1,gy))
      | a > lenX-1 = ((0,y),(gx+1,gy))
      | b < 0 = ((x,lenY-1),(gx,gy-1))
      | b > lenY-1 = ((x,0),(gx,gy+1))
      | otherwise = ((a,b),(gx,gy))
    lenX = V.length . V.head $ g
    lenY = V.length g

findStart :: Garden -> Coord
findStart g = findRow where
    findCol s = foldr (\(i,c) r -> if c == Start then i else r) 0 (zip [0..] (V.toList s))
    findRow = foldr (\(j,s) r -> let i = (findCol s) in if i /= 0 then (i,j) else r) (0,0) (zip [0..] (V.toList g))

bfs :: Int -> Garden -> Int
bfs n g = last . snd $ evalRWS (inner 0 [findStart g]) () () where
    inner :: Int -> [Coord] -> RWS () [Int] () ()
    inner c cur
      | c == n = return ()
      | otherwise = do
        let next = nub . concatMap (findAdjacent g) $ cur
        tell [length next]
        inner (c+1) next

-- type State = ((M.Map (Int, Int) [Int]), (S.Set (Int, Int)))

-- bfs2 :: Int -> Garden -> Int
-- bfs2 n g = sum . fmap (\(_, a) -> head a) . M.toList . fst . fst $ execRWS (inner 0 [((findStart g),(0,0))]) () (M.empty, S.empty) where
-- -- bfs2 n g =  fst $ execRWS (inner 0 [((findStart g),(0,0))]) () (M.empty, S.empty) where
--     inner :: Int -> [Coord2] -> RWS () () State ()
--     inner c cur
--       | c == n = return ()
--       | otherwise = do
--         (historyMap, curCycles) <- get
--         let next = filter (\(_,n) -> n `S.notMember` curCycles) . nub . concatMap (findAdjacent2 g) $ cur
--         let nextGroup = M.fromList $ fmap (\a -> (snd (head a), [length a])) (groupBy ((==) `on` snd) (sortBy (comparing snd) next))
--         let newHistoryMap = M.unionWith (++) nextGroup historyMap
--         let (newHistoryMapCycleChecked, newCycles) = foldr (\(c', is) (h, cc) -> let gp = getPeriodic is in if (length gp) /= 2 then (h,cc) else (M.insert c' (if c `mod` 2 == n `mod` 2 then reverse gp else gp) h, S.insert c' cc)) (newHistoryMap, curCycles) (M.toList newHistoryMap)
--         put (newHistoryMapCycleChecked, newCycles)
--         inner (c+1) next

bfs2 :: Int -> Garden -> Int
bfs2 n g = last . snd $ evalRWS (inner 0 [((findStart g),(0,0))]) () () where
    inner :: Int -> [Coord2] -> RWS () [Int] () ()
    inner c cur
      | c == n = return ()
      | otherwise = do
        let next = nub . concatMap (findAdjacent2 g) $ cur
        tell [length next]
        inner (c+1) next
    
getPeriodic :: [Int] -> [Int]
getPeriodic is = maximum $ (inner 1) where
    inner n
      | n * 2 >= length is = [[]]
      | take n is == take n (drop n is) = [take n is] ++ inner (n+1)
      | otherwise = inner (n+1)

test :: IO ()
test = do
    input <- readFile "data/day21.txt"
    let garden = parseGarden (lines input)
    let part1 = bfs 64 garden 
    print part1
    let l = length garden
    -- let y = bfs2 400 garden
    -- print (snd y)
    -- let res = sum . fmap (\(_, a) -> head a) $ M.toList (fst (fst (x)))
    -- print res
    -- print (getPeriodic [4])
    -- 65, 196, 327
    print l 
    -- let x = bfs2 65 garden
    -- let x2 = bfs2 196 garden
    -- let x3 = bfs2 327 garden

    -- -- let x = bfs2 10 garden
    -- -- let x2 = bfs2 50 garden
    -- -- let x3 = bfs2 100 garden

    -- print x
    -- print x2
    -- print x3


    let part2 = 14590 * (26501365^2) - 14486*26501365 + 3587
    print part2

    -- print part2

    -- let part2test =88 * (2^2) - 66 * 2 + 41

    -- print part2test

    -- let x = bfs2 42 garden

    -- print x


    -- let y = []
    -- let x = maximum y
    -- print x
    
    -- let x = getPeriodic [7,5,3,1]
    -- print x
    -- print x
    -- print x2
    -- print x3

    -- let s = ((findStart garden),(0,0))
    -- let y = findAdjacent2 garden s 
    -- print y