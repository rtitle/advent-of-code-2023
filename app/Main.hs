module Main (main) where

import Utils (runDay)
import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)

main :: IO ()
main = do
    runDay 1 day1
    runDay 2 day2
    runDay 3 day3
    runDay 4 day4
