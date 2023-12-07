module Day7(day7) where

import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)

cardValue :: Bool -> Char -> Int
cardValue _ 'T' = 10
cardValue part1 'J' = if part1 then 11 else 1
cardValue _ 'Q' = 12
cardValue _ 'K' = 13
cardValue _ 'A' = 14
cardValue _ c = digitToInt c

handValue :: Bool -> String -> (Int, Int)
handValue part1 s = (groups !! 0 + numJokers, groups !! 1) where
    cards = filter (\c -> part1 || c /= 'J') . head . splitOn " " $ s
    numJokers = if part1 then 0 else length . filter (=='J') $ s
    groups = take 2 . (++ [0,0]) . sortBy (flip compare) . fmap length . group . sort $ cards
    
totalWinnings :: [String] -> Int
totalWinnings s = foldl inner 0 $ zip [1..] s where
    inner r (i, c) = r + i * (bid c)
    bid = read @Int . head . drop 1 . splitOn " "
    
day7 :: String -> (Int, Int) 
day7 input = (part1, part2) where
    ls = lines input
    part1Compare = comparing (handValue True) <> comparing (fmap (cardValue True))
    part2Compare = comparing (handValue False) <> comparing (fmap (cardValue False))
    part1 = totalWinnings . sortBy part1Compare $ ls
    part2 = totalWinnings . sortBy part2Compare $ ls
