module Main (main) where

import Utils (runDay)
import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Day13 (day13)
import Day14 (day14)
import Day15 (day15)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18)
import Day19 (day19)
-- import Day20 (test)
import Day21 (day21)
import Day22 (day22)
import Day23 (day23)
import Day24 (day24)

main :: IO ()
main = do
    runDay 1 day1
    runDay 2 day2
    runDay 3 day3
    runDay 4 day4
    runDay 5 day5
    runDay 6 day6
    runDay 7 day7
    runDay 8 day8
    runDay 9 day9
    runDay 10 day10
    runDay 11 (day11 False)
    runDay 12 day12
    runDay 13 day13
    runDay 14 day14
    runDay 15 day15
    runDay 16 day16
    -- runDay 17 day17 -- slow
    -- runDay 18 day18 -- slow
    runDay 19 day19
    -- todo 20
    runDay 21 (day21 False 64)
    runDay 22 day22
    runDay 23 day23
    runDay 24 (day24 False)
