module Day6 (day6) where

import Data.List.Split (splitOn)

parseRaces :: Bool -> [String] -> [(Int, Int)]
parseRaces part1 [time, distance] = zip (process time) (process distance) where
    process s = if part1 
        then fmap (read @Int) . drop 1 . filter (not . null) . splitOn " " $ s
        else [read @Int . concat . drop 1 . filter (not . null) . splitOn " " $ s]
parseRaces _ _ = []

waysToWin :: (Int, Int) -> Int
waysToWin (time, distance) = length . filter (> distance) . fmap computeDistance $ [0..time] where
    computeDistance timeSpent = (time - timeSpent) * timeSpent

-- quadratic formula:
-- -x^2 + tx - d = 0
waysToWin2 :: (Int, Int) -> Int
waysToWin2 (time, distance) = abs (fst ans - snd ans) - 1 where
    inner = (time^(2 :: Int)) - 4 * distance
    part = (sqrt (fromIntegral inner)) / (-2 :: Float)
    ans = (-time + ceiling part, -time - floor part) 

day6 :: String -> (Int, Int)
day6 input = (part1, part2) where
    ls = lines input
    racesPart1 = parseRaces True ls
    racesPart2 = parseRaces False ls
    part1 = product . fmap waysToWin $ racesPart1
    -- part2BruteForce = waysToWin . head $ racesPart2
    part2 = waysToWin2 . head $ racesPart2