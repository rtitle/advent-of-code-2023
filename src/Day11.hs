module Day11 (day11) where

import Data.List (tails)

type Coord = (Int, Int)

parseMap :: [String] -> [Coord]
parseMap ls = foldr parseRow [] (zip [0..] ls) where
    parseRow (y, s) r = r ++ foldr (parse y) [] (zip [0..] s)
    parse y (x, '#') r = (x,y):r
    parse _ _ r = r

empty :: [String] -> [Coord] -> ([Int], [Int])
empty ls m = (emptyCols, emptyRows) where
    lenX = length . head $ ls
    lenY = length ls
    xs = fmap fst m
    ys = fmap snd m
    emptyCols = filter (`notElem` xs) [0..lenX-1]
    emptyRows = filter (`notElem` ys) [0..lenY-1]

updateMap :: [Coord] -> [Int] -> [Int] -> Int -> [Coord]
updateMap m emptyCols emptyRows factor = fmap update m where
    emptyColsBefore n = length . filter (<n) $ emptyCols
    emptyRowsBefore n = length . filter (<n) $ emptyRows
    update (x, y) = (x + (emptyColsBefore x) * (factor - 1), y + (emptyRowsBefore y) * (factor - 1))

pairs :: [Coord] -> [(Coord, Coord)]
pairs m = [(x,y) | (x:ys) <- tails m, y <- ys]

dist :: (Coord, Coord) -> Int
dist ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

day11 :: Bool -> String -> (Int, Int)
day11 test input = (part1, part2) where
    ls = lines input
    m = parseMap ls
    (emptyCols, emptyRows) = empty ls m
    updated1 = updateMap m emptyCols emptyRows 2
    part1 = sum . fmap dist . pairs $ updated1
    updated2 = updateMap m emptyCols emptyRows (if test then 100 else 1000000)
    part2 = sum . fmap dist . pairs $ updated2