module Day1 (day1) where

import Control.Applicative (liftA2)

day1Part1 :: String -> Int
day1Part1 input = 0

day1Part2 :: String -> Int
day1Part2 input = 0

day1 :: String -> (Int, Int)
day1 = liftA2 (,) day1Part1 day1Part2