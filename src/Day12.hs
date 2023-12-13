module Day12 (day12) where

import Data.List (group, tails, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Spring = Unknown | Damaged | Operational deriving (Show, Eq, Ord)
data Line = Line {
    springs :: [Spring],
    groups :: [Int] 
} deriving (Show, Eq, Ord)

type Cache = M.Map Line Int

parseLine :: String -> Line
parseLine s =  Line sp g where
    splitted = splitOn " " s
    parseSpring '#' = Damaged
    parseSpring '.' = Operational
    parseSpring _ = Unknown
    sp = fmap parseSpring (splitted !! 0)
    g = fmap read . splitOn "," $ splitted !! 1

valid :: Line -> Bool
valid (Line s g) = damagedGroups s == g

damagedGroups :: [Spring] -> [Int]
damagedGroups = fmap (length) . filter ((==Damaged) . head) . group

perms :: Line -> [Line]
perms (Line s g) = fmap (\c -> Line (merged c) g) chosen where 
    unknowns = M.fromList . filter ((== Unknown) . snd) $ (zip [0..] s)
    damaged = M.fromList . filter ((== Damaged) . snd) $ (zip [0..] s)
    chosen = choose (M.toList unknowns) ((sum g) - (length damaged))
    merged c = fmap (\i -> fromMaybe (fromMaybe Operational (M.lookup i damaged)) (fmap (\_ -> Damaged) (M.lookup i (M.fromList c)))) [0..length s]

choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

countLine :: Line -> Int
countLine = length . filter (valid) . perms

isPossOperational :: Spring -> Bool
isPossOperational Operational = True
isPossOperational Unknown = True
isPossOperational _ = False

isPossDamaged :: Spring -> Bool
isPossDamaged Damaged = True
isPossDamaged Unknown = True
isPossDamaged _ = False

possibleDamagedChunk :: [Spring] -> Int -> Bool
possibleDamagedChunk sps n = isDamagedChunk && ((null afterChunk) || (isPossOperational $ head afterChunk)) where 
    isDamagedChunk = (length $ filter isPossDamaged $ take n sps) == n
    afterChunk = drop n sps

initialCache :: Line -> Cache
initialCache (Line sps signature) = M.union lastOperational cache0
  where cache0 = M.union sprs sigs
        sprs = M.fromList $ fmap (\s -> (Line s [], 0)) $ tails sps
        sigs = M.fromList $ fmap (\g -> (Line [] g, 0)) $ tails signature
        lastOperationalChunk = 
          reverse $ takeWhile isPossOperational $ reverse sps
        lastOperational = 
          M.fromList $ fmap (\s -> (Line s [], 1)) $ tails lastOperationalChunk

fillTable, fillTableSigs, fillTableCell :: Cache -> Line -> Cache
fillTable table (Line sps signatures) = 
  foldr (\ss t -> fillTableSigs t (Line ss signatures)) table $ tails sps 

fillTableSigs table (Line sps signatures) = 
  foldr (\gs t -> fillTableCell t (Line sps gs)) table $ tails signatures

fillTableCell table record
  | record `M.member` table = table
  | otherwise = M.insert record (opN + signN) table
  where (Line sps g) = record
        opN = if (isPossOperational (head sps)) then table M.! (Line (tail sps) g) else 0
        signN = if (possibleDamagedChunk sps (head g)) then table M.! (Line (drop ((head g) + 1) sps) (tail g)) else 0

countViable :: Line -> Int
countViable record = table M.! record
  where table0 = initialCache record
        table = fillTable table0 record

unfoldRecord :: Line -> Line
unfoldRecord (Line sps signature) = Line uSprings uSignature
  where uSprings = intercalate [Unknown] $ replicate 5 sps
        uSignature = concat $ replicate 5 signature

day12 :: String -> (Int, Int)
day12 input = (part1, part2) where
    ls = lines input
    p = fmap parseLine ls
    part1 = sum . fmap countLine $ p
    part2 = sum . fmap (countViable . unfoldRecord) $ p