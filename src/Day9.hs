module Day9 (day9) where

import Data.List (nub)
import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine = fmap (read @Int) . splitOn " "

difference :: [Int] -> [Int]
difference [] = []
difference is = zipWith (-) (tail is) is

extrapolate :: [Int] -> Int
extrapolate is
  | length (nub is) == 1 = head is
  | otherwise = (last is) + extrapolate (difference is)

day9 :: String -> (Int, Int)
day9 input = (part1, part2) where
    ls = fmap parseLine . lines $ input
    part1 = sum . fmap extrapolate $ ls
    part2 = sum . fmap (extrapolate . reverse) $ ls