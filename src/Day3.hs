module Day3 (day3) where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as M

day3 :: String -> (Int, Int)
day3 = liftA2 (,) (process True) (process False)

process :: Bool -> String -> Int
process part1 input = if part1 then p1 else p2 where
    ls = lines input
    allRows = M.unionsWith (++) $ fmap (processRow part1 ls) (zip [0..] ls)
    p1 = sum . concat . (fmap snd) . M.toList $ allRows
    p2 = sum . (fmap product) . (fmap snd) . M.toList . M.filter (\x -> length x == 2) $ allRows

processRow :: Bool -> [String] -> (Int, String) -> M.Map (Int, Int) [Int]
processRow part1 grid (j, s) = processCur res where
    res = foldl inner (M.empty, ([], [])) (zip [0..] s)
    inner r@(acc, (curNum, curAdj)) (i, c) =
        if isDigit c
            then (acc, (curNum ++ [c], nub (curAdj ++ findAdjecent i)))
            else (processCur r, ([], []))
    processCur (acc, (curNum, curAdj)) = foldr (\c r -> M.insertWith (++) c [str2Int curNum] r) acc curAdj
    findAdjecent i = filter (\(x,y) -> test (safeLookup grid x y)) [(x,y) | x<-[i-1, i, i+1], y<-[j-1, j, j+1]]
    isSymbol c = not (c == '.' || isDigit c)
    isGear c = c == '*'
    test c = if part1 then isSymbol c else isGear c

safeLookup :: [String] -> Int -> Int -> Char
safeLookup s x y = s !! (min (lenY-1) (max 0 y)) !! (min (lenX-1) (max 0 x)) where
    lenY = length s
    lenX = length . head $ s 

str2Int :: String -> Int
str2Int [] = 0
str2Int s = read s :: Int