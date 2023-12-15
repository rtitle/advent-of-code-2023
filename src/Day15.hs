module Day15 (day15) where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import qualified Data.Map as M

type Lens = (String, Int)

type Boxes = M.Map Int [Lens]

hash :: String -> Int
hash s = inner 0 s where
    inner n [] = n 
    inner n (a:as) = inner (((n + ord a) * 17) `mod` 256) as

modify :: Boxes -> String -> Boxes
modify bs s = M.insertWith combine h [(label, value)] bs where
    ins = splitOn "=" s 
    rmv = splitOn "-" s
    label = if length ins == 2 then head ins else head rmv
    value = if length ins == 2 then read (ins !! 1) else 0
    h = hash label
    combine a b = if length ins == 2 
        then fromMaybe (b++a) (fmap (\n -> (take (n) b) ++ a ++ (drop (n+1) b)) (elemIndex (fst (head a)) (fmap fst b)))
        else filter (\l -> fst l /= fst (head a)) b

focusPower :: (Int, [Lens]) -> Int
focusPower (bi, ls) = sum . fmap (\(i, l) -> (1+bi) * i * snd l) $ zip [1..] ls

day15 :: String -> (Int, Int)
day15 input = (part1, part2) where
    ls = splitOn "," input
    part1 = sum . fmap hash $ ls
    boxes = foldl modify M.empty ls
    part2 = sum . fmap focusPower . M.toList $ boxes