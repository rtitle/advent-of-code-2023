module Day18 (day18) where

import Control.Monad.RWS (RWS, evalRWS, get, put, tell, execRWS)
import Data.Char (digitToInt)
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Numeric (readHex)

data Direction = U | D | L | R deriving (Eq, Show)
data Instruction = Instruction {
    dir :: Direction, 
    nsteps :: Int
} deriving (Eq, Show)

type Coord = (Int, Int)
type Hole = [Coord]

parseInstructions :: [String] -> [Instruction]
parseInstructions ls = fmap parseInstruction ls where
    parseInstruction s = let sp = splitOn " " s in Instruction (parseDirection (sp !! 0)) (read @Int (sp !! 1))
    parseDirection "D" = D
    parseDirection "U" = U
    parseDirection "L" = L 
    parseDirection _ = R

parseInstructionsPart2 :: [String] -> [Instruction]
parseInstructionsPart2 ls = fmap parseInstruction ls where
    parseInstruction s = Instruction (parseDirection . digitToInt . last . rgb $ s) (fst . head . readHex . init . rgb $ s)
    rgb s = filter (`notElem` "()#") $ (splitOn " " s) !! 2
    parseDirection 0 = R
    parseDirection 1 = D 
    parseDirection 2 = L 
    parseDirection _ = U

initialHole :: Hole
initialHole = [(0,0)]

applyInstruction :: Hole -> Instruction -> Hole 
applyInstruction h (Instruction d n) = (range d) ++ h where
    (x,y) = head h
    range U = fmap (\i -> (x,i)) [y-n..y]
    range D = reverse . fmap (\i -> (x,i)) $ [y..y+n]
    range L = fmap (\i -> (i,y)) [x-n..x]
    range R = reverse . fmap (\i -> (i,y)) $ [x..x+n]

applyInstructionPart2 :: Hole -> Instruction -> Hole
applyInstructionPart2 h (Instruction d n) = (range d) : h where
    (x,y) = head h
    range U = (x,y-n)
    range D = (x,y+n)
    range L = (x-n,y)
    range R = (x+n,y)

flood :: Hole -> Int
flood h = part1 . length . snd $ evalRWS (inner initial) () S.empty where
    inner :: Coord -> RWS () [Coord] (S.Set Coord) ()
    inner (x,y)
      | x == minx || x == maxx || y == miny || y == maxy = return ()
      | (x,y) `elem` h = return ()
      | otherwise = do
        tell [(x,y)]
        seen <- get
        let next = filter (`S.notMember` seen) [(x+1,y), (x-1,y), (x,y-1), (x,y+1)]
        let newSeen = foldr (S.insert) (S.insert (x,y) seen) next
        put newSeen
        _ <- traverse inner next
        return ()
    minx = (minimum (fmap fst h)) - 2
    maxx = (maximum (fmap fst h)) + 2
    miny = (minimum (fmap snd h)) - 2
    maxy = (maximum (fmap snd h)) + 2
    initial = (minx+1, miny+1)
    part1 n = (maxx - minx - 1) * (maxy - miny - 1) - n

getDir :: Coord -> Coord -> Direction
getDir (x1, y1) (x2, y2)
  | x1 < x2 = R
  | x1 > x2 = L 
  | y1 < y2 = D
  | otherwise = U

-- use a better data structure for this than list
clockwise :: Hole -> Int
clockwise h = sum . snd $ execRWS inner () h where
    inner :: RWS () [Int] Hole ()
    inner = do
        -- state <- fmap (collapse 0) get
        state <- get
        if null state || (length state < 4) then return () else case (process state) of 
            Just (newPoint, area) -> do
                tell [area]
                let newState = (drop 4 state) ++ [head state, newPoint, head (drop 3 state)]
                if isSquare newState
                    then tell [squareArea newState]
                    else ((put newState) >> inner)
            Nothing -> do
                put $ (tail state) ++ [head state]
                inner
    process (a@(ax,ay):b@(bx,by):c@(_,cy):d@(dx,dy):_) = 
        let dirs = [(getDir a b), (getDir b c), (getDir c d)] in 
            case (dirs) of 
                [R,D,L] -> let newX = max ax dx 
                               newY = if ax < dx then ay else dy in
                                Just ((newX, newY), (bx - newX) * (cy - by + 1))
                [L,U,R] -> let newX = min ax dx 
                               newY = if ax < dx then dy else ay in
                                Just ((newX, newY), (newX - bx) * (by - cy + 1))
                [D,L,U] -> let newY = max ay dy 
                               newX = if ay > dy then dx else ax in
                                Just ((newX, newY), (ax - dx + 1) * (by - newY))
                [U,R,D] -> let newY = min ay dy 
                               newX = if ay < dy then dx else ax in
                                Just ((newX, newY), (dx - ax + 1) * (newY - by))
                _ -> Nothing
    process _ = Nothing
    isSquare hole = let minX = minimum . fmap fst $ hole
                        maxX = maximum . fmap fst $ hole 
                        minY = minimum . fmap snd $ hole
                        maxY = maximum . fmap snd $ hole in 
                            all (\(x,y) -> (x == minX || x == maxX) && (y == minY || y == maxY)) hole
    squareArea hole = let minX = minimum . fmap fst $ hole
                          maxX = maximum . fmap fst $ hole 
                          minY = minimum . fmap snd $ hole
                          maxY = maximum . fmap snd $ hole in 
                            (maxX - minX + 1) * (maxY - minY + 1)
    collapse n (a@(ax,ay):b@(bx,by):c@(cx,cy):xs)
      | n > length xs = (a:b:c:xs)
      | ax == bx && bx == cx = [a, c] ++ collapse (n+1) (c:xs)
      | ay == by && by == cy = [a, c] ++ collapse (n+1) (c:xs)
      | otherwise = [a,b,c] ++ collapse (n+1) (b:c:xs)
    collapse _ xs = xs

day18 :: String -> (Int, Int)
day18 input = (part1, part2) where
    is = parseInstructions (lines input)
    hole = nub $ foldl applyInstruction initialHole is
    part1 = flood hole
    is2 = parseInstructionsPart2 (lines input)
    hole2 = [(0,0)] ++ (init . reverse . nub $ foldl applyInstructionPart2 initialHole is2)
    -- hole2 = nub $ foldl applyInstructionPart2 initialHole is2
    part2 = clockwise hole2
    