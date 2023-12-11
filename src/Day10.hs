module Day10 (day10) where

import qualified Data.Map as M
import Control.Monad.RWS (RWS, evalRWS, execRWS, put, tell)

data Pipe = NS | EW | NE | NW | SE | SW | Start deriving (Eq, Show)
type Coord = (Int, Int)
type PipeMap = M.Map Coord Pipe

parseMap :: [String] -> (Coord, PipeMap)
parseMap ls = (startPos, M.fromList m) where
    (startPos, m) = execRWS (traverse parseLine (zip [0..] ls)) () (0,0)
    parseLine :: (Int, String) -> RWS () [(Coord, Pipe)] Coord ()
    parseLine (y, s) = fmap head . traverse (inner y) . zip [0..] $ s
    inner :: Int -> (Int, Char) -> RWS () [(Coord, Pipe)] Coord ()
    inner y (x, '-') = tell [((x,y), EW)]
    inner y (x, '|') = tell [((x,y), NS)]
    inner y (x, 'L') = tell [((x,y), NE)]
    inner y (x, 'J') = tell [((x,y), NW)]
    inner y (x, '7') = tell [((x,y), SW)]
    inner y (x, 'F') = tell [((x,y), SE)]
    inner y (x, 'S') = do
        put (x, y)
        tell [((x,y), Start)]
    inner _ _ = tell []

connections :: PipeMap -> Coord -> [Coord]
connections m (x,y) = connect (M.lookup (x,y) m) where
    connect (Just NS) = [(x, y-1), (x, y+1)]
    connect (Just EW) = [(x-1, y), (x+1, y)]
    connect (Just NE) = [(x, y-1), (x+1, y)]
    connect (Just NW) = [(x, y-1), (x-1, y)]
    connect (Just SW) = [(x, y+1), (x-1, y)]
    connect (Just SE) = [(x, y+1), (x+1, y)]
    connect _ = []

possibleStartConnections :: PipeMap -> Coord -> [Coord]
possibleStartConnections m startPos = filter (\s -> startPos `elem` (connections m s)) (surrounding startPos) where
    surrounding (x,y) = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

follow :: PipeMap -> Coord -> Coord -> [Coord]
follow m start next = snd $ evalRWS (inner next (getNext start next)) () () where
    inner :: Coord -> [Coord] -> RWS () [Coord] () ()
    inner p [n]
      | n == start = tell [n]
      | otherwise = do
        tell [n]
        inner n (getNext p n)
    inner _ _ = tell []
    getNext prev cur = filter (\c -> c /= prev) $ (connections m cur)

enclosed :: PipeMap -> [String] -> [Coord] -> [Coord]
enclosed m ls cs = foldr inner [] [0..lenY] where
    inner c r = r ++ (snd (foldr (inner2 c) ((0 :: Int, []), []) [0..lenX]))
    inner2 y x ((n, s), r) = if (x,y) `notElem` cs
        then ((n,[]), if n `mod` 2 == 1 then (x,y):r else r)
        else if (m M.! (x,y)) == EW then ((n,s), r)
        else if (m M.! (x,y)) == NS then ((n+1, []), r)
        else if (m M.! (x,y)) == NW then ((n+1, [NW]), r)
        else if (m M.! (x,y)) == SW then ((n+1, [SW]), r)
        else if (m M.! (x,y)) == NE && s == [NW] then ((n+1, []), r)
        else if (m M.! (x,y)) == NE && s == [SW] then ((n, []), r)
        else if (m M.! (x,y)) == SE && s == [SW] then ((n+1, []), r)
        else if (m M.! (x,y)) == SE && s == [NW] then ((n, []), r)
        else ((n+1,[]), r)
    lenX = length . head $ ls
    lenY = length ls

day10 :: String -> (Int, Int)
day10 input = (part1, part2) where
    ls = lines input
    (startPos, pipeMap) = parseMap ls
    p = possibleStartConnections pipeMap startPos
    path = (head p) : (follow pipeMap startPos (head p))
    part1 = (length path) `div` 2
    part2 = length $ enclosed pipeMap ls path