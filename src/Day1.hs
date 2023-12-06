module Day1 (day1) where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)

day1 :: String -> (Int, Int)
day1 = liftA2 (,) (doDay1 True) (doDay1 False)

doDay1 :: Bool -> String -> Int
doDay1 part1 input = sum $ fmap (read @Int . firstAndLast) (lines input) where
    firstAndLast s = if part1 
        then let d = filter isDigit s in [head d, last d]
        else let t = init . tails $ s in [findFirstNum t, findFirstNum (reverse t)]

findFirstNum :: [String] -> Char
findFirstNum [] = '0'
findFirstNum (t:tl) = if isDigit (head t) then head t
        else if isPrefixOf "one" t then '1'
        else if isPrefixOf "two" t then '2'
        else if isPrefixOf "three" t then '3'
        else if isPrefixOf "four" t then '4'
        else if isPrefixOf "five" t then '5'
        else if isPrefixOf "six" t then '6'
        else if isPrefixOf "seven" t then '7'
        else if isPrefixOf "eight" t then '8'
        else if isPrefixOf "nine" t then '9'
        else findFirstNum tl