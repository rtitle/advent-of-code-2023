module Day23 (test) where

import qualified Data.Set as S
import qualified Data.Vector as V

data Dir = N | S | E | W deriving (Eq, Show)
data Node = Path | Forest | Slope Dir deriving (Eq, Show)
type Map = V.Vector (V.Vector Node)
type Coord = (Int, Int)

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
neighbors part1 m (x,y) = filter processNext (if part1 then getNextPart1 else getNextPart2) where
    getNextPart1 = case (m V.! y V.! x) of
        Slope N -> [(x,y-1)] 
        Slope S -> [(x,y+1)]
        Slope E -> [(x+1,y)]
        Slope W -> [(x-1,y)]
        _ -> [(x,y-1), (x,y+1), (x+1,y), (x-1,y)]
    getNextPart2 = [(x,y-1), (x,y+1), (x+1,y), (x-1,y)]
    processNext (nx, ny) = nx >= 0 && nx <= lenX-1 && ny >= 0 && ny <= lenY-1 && (m V.! ny V.! nx) /= Forest
    lenX = V.length (V.head m)
    lenY = V.length m

dfs :: Bool -> Map -> Int
dfs part1 m = (inner start S.empty) - 1 where
    inner :: Coord -> S.Set Coord -> Int
    inner cur visited
      | cur == (end m) = S.size visited
      | otherwise = 
          let next = fmap (\n -> inner n (S.insert n visited)) . filter (`S.notMember` visited) $ (neighbors part1 m cur) in
              if null next then 0 else maximum next 

test :: IO ()
test = do
    input <- readFile "data/day23.txt"
    let m = parseMap (lines input)
    let part1 = dfs True m
    print part1
    let part2 = dfs False m
    print part2
