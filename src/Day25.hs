module Day25 (day25) where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Control.Monad
import qualified Data.Set as S 
import qualified Data.Map as M

type Graph = M.Map String [String]

parseLine :: String -> [(String, String)]
parseLine s = res where
    sp1 = splitOn ":" $ s
    sp2 = splitOn " " $ sp1 !! 1
    res = fmap (\a -> (sp1 !! 0, a)) (drop 1 sp2)

_toGraphiz :: [(String, String)] -> IO ()
_toGraphiz ls = do
    putStrLn "graph {"
    void $ traverse (\(a,b) -> putStrLn ("    " ++ a ++ "--" ++ b ++ ";")) ls 
    putStrLn "}"

toGraph :: [(String, String)] -> Graph
toGraph ls = foldr inner M.empty ls where
    inner (a,b) r = M.unionWith (++) r (M.fromList ([(a, [b])] ++ fmap (\_ -> (b, [a])) b) )

size :: Graph -> Int
size g = inner [fst . head . M.toList $ g] S.empty where
    inner cur visited = 
        let next = filter (`S.notMember` visited) . concatMap (\a -> fromMaybe [] $ M.lookup a g) $ cur
            newVisited = foldr (S.insert) visited next in 
                if null next then S.size newVisited else inner next newVisited

day25 :: Bool -> String -> (Int, Int)
day25 test input = (part1, 0) where
    parsed = concatMap parseLine (lines input)
    -- _toGraphiz parsed
    -- $ dot -Tsvg out.dot -Kneato > out.svg
    removeNodes = if test then [("hfx", "pzl"), ("bvb", "cmg"), ("nvd", "jqt")] else [("hlx", "cpq"), ("spk", "hqp"), ("zlx", "chr")] 
    graph = toGraph parsed
    newGraph = toGraph . filter (`notElem` (removeNodes ++ fmap swap removeNodes)) $ parsed
    s1 = size graph
    s2 = size newGraph
    part1 = s2 * (s1 - s2)