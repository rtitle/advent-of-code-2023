module Day18 (test) where

import Control.Monad.Loops (iterateUntilM)
import Control.Monad.RWS (RWS, runRWS, evalRWS, get, put, tell)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (groupBy, nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
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
    range U = (x,y-n) -- fmap (\i -> (x,i)) [y-n..y]
    range D = (x,y+n) -- reverse . fmap (\i -> (x,i)) $ [y..y+n]
    range L = (x-n,y) -- fmap (\i -> (i,y)) [x-n..x]
    range R = (x+n,y) -- reverse . fmap (\i -> (i,y)) $ [x..x+n]

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
        traverse inner next
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

clockwise :: Hole -> (Hole, [Int])
clockwise h = evalRWS (inner h) () ([], Nothing) where  -- runRWS (iterateUntilM null inner h) () [] where
    inner :: Hole -> RWS () [Int] ([Coord], Maybe (Direction, Int, Coord)) Hole
    -- inner [a, b] = do
    --     tell [abs ((fst a) - (fst b)) + ((snd a) - (snd b))]
    --     return []
    inner [] = do
        (rem, maybeDelta) <- get
        -- traverse (\(d, n, (cx, cy)) -> tell [-n]) maybeDelta
        put ([], Nothing)
        return $ if rem == [(0,0), (0,0)] then [] else rem
        -- if (length rem == 2) then return [] else return rem
    inner (c:cs) = do
        (prev, maybeDelta) <- get
        let delta = fromMaybe 0 (fmap (applyDelta c) maybeDelta)
        -- tell [-delta]
        let maybeP = process c prev
        case (maybeP) of 
            Just (n, p, dir, delta, b) -> do
                tell [n] 
                put ([], if b then Just (dir, delta, c) else Nothing)
                next <- inner cs
                return (p ++ next)
            Nothing -> do
                put ((prev ++ [c]), Nothing)
                inner cs

    applyDelta :: Coord -> (Direction, Int, Coord) -> Int
    applyDelta (newx,newy) (lastDir, n, (cx,cy)) = res where
        curDir = getDir (cx, cy) (newx, newy)
        res = case ((lastDir, curDir)) of
            (L, U) -> min n (cy-newy)
            (R, D) -> min n (newy-cy)
            (U, R) -> min n (newx-cx)
            (D, L) -> min n (cx-newx)
            otherwise -> 0

    process :: Coord -> [Coord] -> Maybe (Int, [Coord], Direction, Int, Bool)
    process c p 
      | (length p) >= 3 = 
        let (x,y,z) = last3 p
            pTrunc = removeLast2 p
            dirs = [(getDir x y), (getDir y z), (getDir z c)] 
            dxs = getDxs x y z c
            dys = getDys x y z c
            minDy = minimum dys
            minDx = minimum dxs
            -- delta = if (length dxs) == 2 then head dys else if (length dys == 2) then head dxs else 0
            n = minDx * minDy -- - delta
            -- sq = isSquare x y z c
            -- per = perim x y z c
            -- n = (dxs x y z c) * (dys x y z c) + (if sq then per else 0)
        in 
            case (dirs) of 
                [R,D,L] -> Just (n, pTrunc ++ [((fst y)-minDx+1, snd x), c], L, minDy, (last dxs) == minDx)  -- down keep, up subtract by (cy-newy)
                [L,U,R] -> Just (n, pTrunc ++ [(fst c, snd y)], R, minDy, (last dxs) == minDx)  -- up keep, down subtract by (newy-cy)
                [D,L,U] -> Just (n, pTrunc ++ [(fst y, snd c)], U, minDx, (last dys) == minDy)  -- left keep, right subtract (newx-cx)
                [U,R,D] -> Just (n, pTrunc ++ [(fst y, snd c)], D, minDx, (last dys) == minDy)  -- right keep, left subtract by (cx-newx)
                otherwise -> Nothing
      | otherwise = Nothing
    last3 p = (last . init . init $ p, last . init $ p, last p)
    removeLast2 p = init . init $ p
    clockWiseDirs = [[R, D, L], [L, U, R], [D, L, U], [U, R, D]]
    getDxs (x1, y1) (x2, y2) (x3, y3) (cx, cy) = nub . fmap (+1) . filter (/=0) $ [abs (x2-x1), abs (x3-x2), abs (cx-x3)]
    getDys (x1, y1) (x2, y2) (x3, y3) (cx, cy) = nub . fmap (+1) . filter (/=0) $ [abs (y2-y1), abs (y3-y2), abs (cy-y3)]
    -- isSquare (x1, y1) (x2, y2) (x3, y3) (cx, cy) = y1 == y2 && x1 == cx && x2 == x3 && y2 == y1 && y3 == cy && x3 == x2
    -- perim (x1, y1) (x2, y2) (x3, y3) (cx, cy) = (x2-x1) + (y3-y2) + (x3-cx) + (cy-y1)
    

test :: IO ()
test = do
    input <- readFile "test/data/day18.txt"
    let is = parseInstructions (lines input)
    let hole = nub $ foldl applyInstruction initialHole is
    let part1 = flood hole
    print part1
    let is2 = parseInstructionsPart2 (lines input)
    let hole2 = [(0,0)] ++ (init . reverse . nub $ foldl applyInstructionPart2 initialHole is)
    -- let hole2 = [(0,0), (6,0), (6,5), (4,5), (4,2), (0,2)]
    -- let test2 = [(0,0),(4,0),(4,2),(0,2)]
    -- let test3 = [(0,0),(0,0)]
    -- let test4 = [(0,0)]

    -- let test = [(0,0),(461937,0),(461937,56407), (0, 56407)]
    -- let test = [(0,0), (1,0), (1,1), (2,1), (2,2), (0,2)]

    print hole2
    let part2 = clockwise hole2
    print part2
    -- print ((461937+1) * (56407+1))
