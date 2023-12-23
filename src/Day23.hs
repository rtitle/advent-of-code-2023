module Day23 (day23) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

data Dir = N | S | E | W deriving (Eq, Show)
data Node = Path | Forest | Slope Dir deriving (Eq, Show)
type Map = V.Vector (V.Vector Node)
type Coord = (Int, Int)
type Graph = M.Map Coord [(Int, Coord)]

parseMap :: [String] -> Map
parseMap = V.fromList . fmap parseRow where
    parseRow = V.fromList . fmap parseNode
    parseNode '#' = Forest
    parseNode '^' = Slope N
    parseNode 'v' = Slope S
    parseNode '<' = Slope W
    parseNode '>' = Slope E
    parseNode _ = Path

start :: Coord
start = (2, 0)

end :: Map -> Coord
end m = (lenX-2, lenY-1) where
    lenX = V.length (V.head m)
    lenY = V.length m

neighbors :: Bool -> Map -> Coord -> [Coord]
neighbors ignoreSlope m (x,y) = filter processNext (if ignoreSlope then getNextIgnoreSlope else getNext) where
    getNext = case (m V.! y V.! x) of
        Slope N -> [(x,y-1)] 
        Slope S -> [(x,y+1)]
        Slope E -> [(x+1,y)]
        Slope W -> [(x-1,y)]
        _ -> [(x,y-1), (x,y+1), (x+1,y), (x-1,y)]
    getNextIgnoreSlope = [(x,y-1), (x,y+1), (x+1,y), (x-1,y)]
    processNext (nx, ny) = nx >= 0 && nx <= lenX-1 && ny >= 0 && ny <= lenY-1 && (m V.! ny V.! nx) /= Forest
    lenX = V.length (V.head m)
    lenY = V.length m

dfs :: Map -> Int
dfs m = (inner start S.empty) - 1 where
    inner :: Coord -> S.Set Coord -> Int
    inner cur visited
      | cur == (end m) = S.size visited
      | otherwise = 
          let next = fmap (\n -> inner n (S.insert n visited)) . filter (`S.notMember` visited) $ (neighbors False m cur) in
              if null next then 0 else maximum next 

-- graph of junctures to reduce search space
buildGraph :: Map -> Graph
buildGraph m = foldr loopRow M.empty [0..lenY-1] where
    loopRow y r = foldr (inner y) r [0..lenX-1]
    inner y x r = if isJuncture (x,y) then M.insert (x,y) (acc S.empty 0 (x,y)) r else r    --(concatMap (\d -> acc d 1 (step d (x,y))) [N,S,E,W]) r else r 
    isJuncture coord = length (neighbors True m coord) > 2 || coord == start || coord == (end m)
    acc visited n (x,y)
      | n == 0 = go
      | (m V.! y V.! x) == Forest = []
      | isJuncture (x,y) = [(n, (x,y))]
      | otherwise = go where 
        go = concatMap (acc (S.insert (x,y) visited) (n+1)) . filter (`S.notMember` visited) $ (neighbors True m (x,y))
    lenX = V.length (V.head m)
    lenY = V.length m

dfsPart2 :: Map -> Graph -> Int
dfsPart2 m g = (inner start 0 S.empty) - 1 where
    inner :: Coord -> Int -> S.Set Coord -> Int
    inner cur n visited
      | cur == (end m) = n
      | otherwise = 
          let next = fmap (\(a,b) -> inner b (n+a) (S.insert b visited)) . filter (\(_,b) -> b `S.notMember` visited) $ g M.! cur in
            if null next then 0 else maximum next 

day23 :: String -> (Int, Int)
day23 input = (part1, part2) where
    m = parseMap (lines input)
    part1 = dfs m
    graph = buildGraph m
    part2 = dfsPart2 m graph
