module Day9 (day9) where

import Data.List (nub)
import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine = fmap (read @Int) . splitOn " "

difference :: [Int] -> [Int]
difference [] = []
difference is = zipWith (-) (tail is) is

applyDifference :: Bool -> [Int] -> Int
applyDifference part1 is
  | length (nub is) == 1 = head is
  | part1 = (last is) + applyDifference part1 (difference is)
  | otherwise = (head is) - applyDifference part1 (difference is)

day9 :: String -> (Int, Int)
day9 input = (part1, part2) where
    ls = fmap parseLine . lines $ input
    part1 = sum . fmap (applyDifference True) $ ls
    part2 = sum . fmap (applyDifference False) $ ls