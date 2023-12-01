module Day1 (day1) where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)

day1Part1 :: String -> Int
day1Part1 input = sum ns where
    ls = lines input
    digits = fmap (filter isDigit) ls
    firstAndLast = fmap (\s -> [head s, last s]) digits
    ns = fmap str2Int firstAndLast

day1Part2 :: String -> Int
day1Part2 input = sum ns where
    ls = lines input
    firstAndLast = fmap (\s -> (findFirstNum (tails s)) ++ findFirstNum (reverse . tails $ s)) ls
    ns = fmap str2Int firstAndLast

str2Int :: String -> Int
str2Int s = read s :: Int

findFirstNum :: [String] -> String
findFirstNum [] = ""
findFirstNum ("":tl) = findFirstNum tl
findFirstNum (t:tl) = if isDigit (head t) then [head t]
        else if isPrefixOf "one" t then "1"
        else if isPrefixOf "two" t then "2"
        else if isPrefixOf "three" t then "3"
        else if isPrefixOf "four" t then "4"
        else if isPrefixOf "five" t then "5"
        else if isPrefixOf "six" t then "6"
        else if isPrefixOf "seven" t then "7"
        else if isPrefixOf "eight" t then "8"
        else if isPrefixOf "nine" t then "9"
        else findFirstNum tl

day1 :: String -> (Int, Int)
day1 = liftA2 (,) day1Part1 day1Part2