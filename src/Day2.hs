module Day2 (day2) where

import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.List.Split (splitOn)

-- overly nested data structure
data Color = Red | Green | Blue deriving (Show, Eq)
type Cube = (Color, Int)
type Grab = [Cube]
type Game = (Int, [Grab])

day2 :: String -> (Int, Int)
day2 input = (part1, part2) where
    games = fmap parseLine (lines input)
    part1 = sum $ doPart1 games
    part2 = sum $ doPart2 games

str2Color :: String -> Color
str2Color "red" = Red
str2Color "green" = Green
str2Color "blue" = Blue
str2Color _ = Red

parseLine :: String -> Game
parseLine s = (game, grabs) where
    gameSplit = splitOn ":" s
    game = read @Int . drop 5 $ (gameSplit !! 0)
    grabsSplit = splitOn ";" (gameSplit !! 1)
    cubesSplit = fmap (splitOn ",") grabsSplit
    parseCube cubeStr = let a = splitOn " " cubeStr in (str2Color (a !! 2), read @Int (a !! 1))
    grabs = fmap (fmap parseCube) cubesSplit

cubeExceedsBoundary :: Cube -> Bool
cubeExceedsBoundary (Red, n) = n > 12
cubeExceedsBoundary (Green, n) = n > 13
cubeExceedsBoundary (Blue, n) = n > 14

doPart1 :: [Game] -> [Int]
doPart1 gs = snd $ evalRWS (traverse inner gs) () () where 
    inner :: Game -> RWS () [Int] () ()
    inner (game, grabs) = if any (any cubeExceedsBoundary) grabs then tell [] else tell [game]

doPart2 :: [Game] -> [Int]
doPart2 gs = fmap inner gs where
    inner (_, grabs) = let c = concat grabs in 
        (maxColor Red c) * (maxColor Green c) * (maxColor Blue c)
    maxColor clr = maximum . (fmap snd) . (filter (\c -> fst c == clr))