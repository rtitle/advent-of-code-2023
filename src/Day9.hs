module Day9 (day9) where

import Data.List (nub)
import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine = fmap (read @Int) . splitOn " "

difference :: [Int] -> [Int]
difference [] = []
difference is = zipWith (-) (tail is) is

applyDifference :: [Int] -> Int
applyDifference is
  | length (nub is) == 1 = head is
  | otherwise = (last is) + applyDifference (difference is)

day9 :: String -> (Int, Int)
day9 input = (part1, part2) where
    ls = fmap parseLine . lines $ input
    part1 = sum . fmap applyDifference $ ls
    part2 = sum . fmap (applyDifference . reverse) $ ls