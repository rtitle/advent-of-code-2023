module Day22 (test) where

import Data.List ((\\))
import Data.List.Split (splitOn)

type Coord3 = (Int, Int, Int)
type Brick = (Coord3, Coord3)

parseBrick :: String -> Brick
parseBrick s = let sp = fmap parseCoord . splitOn "~" $ s in (sp !! 0, sp !! 1) where
    parseCoord c = let sp = fmap read . splitOn "," $ c in (sp !! 0, sp !! 1, sp !! 2)

rangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlap (a1,a2) (b1,b2) = (b1 >= a1 && b1 <= a2) || (b2 >= a1 && b2 <= a2) || (a1 >= b1 && a2 <= b2) || (b1 >= a1 && b2 <= a2)

third :: (a, b, c) -> c
third (_, _, z) = z

overlap :: Brick -> Brick -> Bool
overlap ((lx1, ly1, lz1), (rx1, ry1, rz1)) ((lx2, ly2, lz2), ((rx2, ry2, rz2))) = rangeOverlap (lx1, rx1) (lx2, rx2) && rangeOverlap (ly1, ry1) (ly2, ry2) && rangeOverlap (lz1, rz1) (lz2, rz2)

overlapXy :: Brick -> Brick -> Bool
overlapXy ((lx1, ly1, _), (rx1, ry1, _)) ((lx2, ly2, _), ((rx2, ry2, _))) = rangeOverlap (lx1, rx1) (lx2, rx2) && rangeOverlap (ly1, ry1) (ly2, ry2)

top :: Brick -> Int
top = third . snd 

bottom :: Brick -> Int
bottom = third . fst

fall :: Brick -> Brick
fall ((x1, y1, z1), (x2, y2, z2)) = ((x1, y1, z1-1), (x2, y2, z2-1))

simulate :: [Brick] -> Int -> [Brick]
simulate [] _ = []
simulate (b:bs) n 
  | n == length (b:bs) * 2 = (b:bs)
  | (bottom b) == 1 = simulate (bs ++ [b]) (n+1)
  | otherwise = 
    let b' = fall b in
        if any (\a -> overlap a b') bs then
            simulate (bs ++ [b]) (n+1)
        else 
            simulate (bs ++ [b']) 0

doPart1 :: [Brick] -> Int
doPart1 bs = length (bs \\ res) where
    res = foldl inner [] bs 
    inner r c = let allUnder = filter (\a -> (top a) == (bottom c) - 1 && overlapXy c a) bs in
        if (length allUnder) == 1 then r ++ allUnder else r

test :: IO ()
test = do
    input <- readFile "data/day22.txt"
    let bricks = fmap parseBrick (lines input)
    let fallen = simulate bricks 0
    let part1 = doPart1 fallen
    print part1