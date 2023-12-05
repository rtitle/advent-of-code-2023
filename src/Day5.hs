module Day5(day5) where

import Data.List (find)
import Data.List.Split (chunksOf, splitOn, splitWhen)
import Data.Maybe (fromMaybe)

data Mapping = Mapping {
    dest :: Int,
    src :: Int,
    range :: Int
} deriving (Eq, Show)

type Seed = Int

data SeedRange = SeedRange {
    seedSrc :: Int,
    seedRange :: Int
} deriving (Eq, Show)

parseSeeds :: [String] -> [Seed]
parseSeeds = fmap str2Int . splitOn " " . drop 7 . head

parseSeedRanges :: [String] -> [SeedRange]
parseSeedRanges = fmap parseSeed . chunksOf 2 . parseSeeds where
    parseSeed a = SeedRange (a !! 0) (a !! 1)

parseAlmanac :: [String] -> [Mapping]
parseAlmanac = fmap (parseMapping . split) . drop 1 where 
    split s = fmap str2Int . splitOn " " $ s
    parseMapping a = Mapping (a !! 0) (a !! 1) (a !! 2)

findSeed :: Seed -> [Mapping] -> Seed
findSeed n mapping = fromMaybe n . fmap getDest . find srcPred $ mapping where
    srcPred (Mapping _ s r) = n >= s && n <= s + r - 1
    getDest (Mapping d s _) = d + (n - s)

findRange :: SeedRange -> [Mapping] -> [SeedRange]
findRange initialSeed mapping = unmapped ++ mapped where
    (unmapped, mapped) = foldl inner ([initialSeed], []) mapping
    inner (u, acc) c = foldl combine ([], acc) (fmap (overlap c) u)
    combine (r1, r2) (c1, c2) = (r1 ++ c1, r2 ++ c2)
    overlap (Mapping md ms mr) seed@(SeedRange s r)
        | s >= (ms + mr) || ms >= (s + r) = ([seed], [])
        | s >= ms && (s + r) <= (ms + mr) = ([], [SeedRange (md + (s - ms)) r])
        | s <= ms && (s + r) >= (ms + mr) = ([SeedRange s (ms - s), SeedRange (ms + mr) (r - (ms + mr - s))], [SeedRange md mr])
        | ms <= s && (ms + mr) > s && (ms + mr) <= (s + r) = ([SeedRange (ms + mr) (r - (ms + mr - s))], [SeedRange (md + (s - ms)) (mr - (s - ms))])
        | ms >= s && ms < (s + r) && (ms + mr) >= (s + r) = ([SeedRange s (ms - s)], [SeedRange md (r - (ms - s))])
        | otherwise = ([], [])

doPart1 :: [[Mapping]] -> [Int] -> Int
doPart1 almanac = minimum . fmap findAll where
    findAll n = foldl findSeed n almanac where

doPart2 :: [[Mapping]] -> [SeedRange] -> Int
doPart2 almanac seeds = minimum . fmap seedSrc $ foldl inner seeds almanac where
    inner r c = concat $ fmap (\x -> findRange x c) r

str2Int :: String -> Int
str2Int s = read s :: Int

day5 :: String -> (Int, Int)
day5 input = (part1, part2) where
    ls = splitWhen null $ lines input
    seeds = parseSeeds . head $ ls
    seedRanges = parseSeedRanges . head $ ls
    mappings = fmap (\n -> parseAlmanac . head . drop n $ ls) [1..7]
    part1 = doPart1 mappings seeds
    part2 = doPart2 mappings seedRanges