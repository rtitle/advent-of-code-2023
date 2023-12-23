module Day23 (test) where

import qualified Data.Vector as V

data Dir = N | S | E | W deriving (Eq, Show)
data Node = Path | Forest | Slope Dir deriving (Eq, Show)
type Map = V.Vector (V.Vector Node)

parseMap :: [String] -> Map
parseMap = V.fromList . fmap parseRow where
    parseRow = V.fromList . fmap parseNode
    parseNode '#' = Forest
    parseNode '^' = Slope N
    parseNode 'v' = Slope S
    parseNode '<' = Slope W
    parseNode '>' = Slope E
    parseNode _ = Path

test :: IO ()
test = do
    input <- readFile "test/data/day23.txt"
    let map = parseNode (lines input)