module Day13 (day13) where

import Data.List (transpose)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe, listToMaybe)

getReflection :: [String] -> [Int]
getReflection s = filter (/=0) . filter (\x -> check (x-1) x) $ [1..len] where
    check a b
      | a == 0 = s !! 0 == s !! b
      | b == len = s !! a == s !! len
      | otherwise = s !! a == s !! b && check (a-1) (b+1)
    len = length s - 1

getEitherReflection :: Int -> [String] -> Int
getEitherReflection orig s = if v > 0 then v else h where
    v = fromMaybe 0 . listToMaybe . filter (/=orig) . fmap (*100) . getReflection $ s
    h = fromMaybe 0 . listToMaybe . filter (/=orig) . getReflection . transpose $ s

smudges :: [String] -> [[String]]
smudges ls = fmap sumdgeAll $ [(x,y) | x <- [0..lenX-1], y <- [0..lenY-1]] where 
    sumdgeAll (x,y) = let (a,b) = splitAt y ls in a ++ (fmap (smudgeRow x) (take 1 b)) ++ (drop 1 b)
    smudgeRow x s = let (a,b) = splitAt x s in a ++ (fmap smudge (take 1 b)) ++ (drop 1 b)
    smudge '#' = '.'
    smudge '.' = '#'
    smudge c = c
    lenX = length . head $ ls
    lenY = length ls

day13 :: String -> (Int, Int)
day13 input = (part1, part2) where
    ls = splitWhen null (lines input)
    orig = fmap (\s -> (s, getEitherReflection 0 s)) ls
    part1 = sum . fmap (snd) $ orig
    part2 = sum . fmap (\(s,o) -> head . filter (/= 0) . fmap (getEitherReflection o) . smudges $ s) $ orig