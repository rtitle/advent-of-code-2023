module Day8 (day8) where

import Control.Monad.RWS (RWS, evalRWS, get, put, tell)
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map as M

parseNode :: String -> (String, (String, String))
parseNode s = (head split1, (head split2, head . drop 1 $ split2)) where
    split1 = splitOn "=" . filter (`notElem` "() ") $ s
    split2 = splitOn "," (split1 !! 1)

countSteps :: Bool -> String -> M.Map String (String, String) -> String -> Int
countSteps part1 instructions nodes start = sum . snd $ evalRWS (inner start) () (concat . repeat $ instructions) where
    isEndNode = if part1 then (=="ZZZ") else (=='Z') . last
    inner :: String -> RWS () [Int] String ()
    inner x
      | isEndNode x = tell [0]
      | otherwise = do
        curIs <- get
        let (ln, rn) = nodes M.! x
        let next = if (head curIs) == 'L' then ln else rn
        tell [1]
        put (tail curIs)
        inner next

lcmm :: [Int] -> Int
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

day8 :: String -> (Int, Int)
day8 input = (part1, part2) where
    ls = splitWhen null . lines $ input
    is = head . head $ ls
    nodes = M.fromList . fmap parseNode . head . drop 1 $ ls
    part1 = countSteps True is nodes "AAA"
    startNodes = filter ((=='A') . last) . M.keys $ nodes
    part2 = lcmm . fmap (countSteps False is nodes) $ startNodes