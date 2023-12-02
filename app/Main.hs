module Main (main) where

import Utils (runDay)
import Day1 (day1)
import Day2 (day2)

main :: IO ()
main = do
    runDay 1 day1
    runDay 2 day2
