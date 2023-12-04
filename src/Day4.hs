{-# LANGUAGE TupleSections #-}

module Day4(day4) where

import Data.List.Split(splitOn)
import qualified Data.Map as M

data Card = Card {
    cid :: Int,
    winning :: [Int],
    drawn :: [Int]
} deriving (Eq, Show)

day4 :: String -> (Int, Int)
day4 input = (part1, part2) where
    cards = fmap parseLine (lines input)
    part1 = sum . fmap doPart1 $ cards
    part2 = doPart2 cards

parseLine :: String -> Card
parseLine s = Card gid (parseNums w) (parseNums d) where
    gameSplit = splitOn ":" s
    gid = str2Int . drop 5 . head $ gameSplit
    numberSplit = splitOn "|" . head . drop 1 $ gameSplit
    w = head numberSplit
    d = head . drop 1 $ numberSplit
    parseNums a = fmap str2Int . filter (not . null) . splitOn " " $ a

doPart1 :: Card -> Int
doPart1 (Card _ w d) = score . length . filter (`elem` w) $ d where
    score 0 = 0
    score n = 2^(n-1)

doPart2 :: [Card] -> Int
doPart2 cs = sum . fmap snd . M.toList $ finalCards where
    initial = M.fromList . fmap (,1) . fmap cid $ cs
    inner r (Card i w d) = foldr (M.adjust (+ (r M.! i))) r [i+1..i+(numWinner w d)]
    numWinner w = length . filter (`elem` w)
    finalCards = foldl inner initial cs

str2Int :: String -> Int
str2Int s = read s :: Int