module Day6 (day6) where

import Data.List.Split (splitOn)
import Utils (str2Int)

parseRaces :: Bool -> [String] -> [(Int, Int)]
parseRaces part1 [time, distance] = zip (process time) (process distance) where
    process s = if part1 
        then fmap str2Int . drop 1 . filter (not . null) . splitOn " " $ s
        else [str2Int . concat . drop 1 . filter (not . null) . splitOn " " $ s]
parseRaces _ _ = []

waysToWin :: (Int, Int) -> Int
waysToWin (time, distance) = length . filter (> distance) . fmap computeDistance $ [0..time] where
    computeDistance timeSpent = (time - timeSpent) * timeSpent

day6 :: String -> (Int, Int)
day6 input = (part1, part2) where
    ls = lines input
    racesPart1 = parseRaces True ls
    racesPart2 = parseRaces False ls
    part1 = product . fmap waysToWin $ racesPart1
    part2 = waysToWin . head $ racesPart2