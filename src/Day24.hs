module Day24 (day24) where

import Data.List.Split (splitOn)
import Math.LinearEquationSolver
import System.IO.Unsafe (unsafePerformIO)

type Coord2 = (Double, Double)
type Coord3 = (Double, Double, Double)
type Speed3 = (Double, Double, Double)
type Hailstone = (Coord3, Speed3)

parseHailstone :: String -> Hailstone
parseHailstone s = ((coords !! 0, coords !! 1, coords !! 2), (speed !! 0, speed !! 1, speed !! 2)) where
    nospaces = filter (/=' ') s
    sp1 = splitOn "@" nospaces
    sp2 s' = fmap read . splitOn "," $ s'
    coords = sp2 (sp1 !! 0)
    speed = sp2 (sp1 !! 1)

findIntercept :: Hailstone -> Hailstone -> Coord2
findIntercept h1 h2 = (x,y) where
    m (_, (run, rise, _)) = rise/run
    b h@((x1, y1, _), _) = -x1 * (m h) + y1
    x = ((b h2) - (b h1)) / ((m h1) - (m h2))
    y = (m h1) * x + (b h1)

choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

inBounds :: Double -> Double -> Hailstone -> Hailstone -> Coord2 -> Bool
inBounds mn mx h1 h2 (x,y) = x >= mn && x <= mx && y >= mn && y <= mx && futureX h1 && futureX h2 && futureY h1 && futureY h2 where
    futureX ((ix, _, _), (irun, _, _)) = (ix < x && irun > 0) || (ix > x && irun < 0) || ix == x
    futureY ((_, iy, _), (_, irise, _)) = (iy < y && irise > 0) || (iy > y && irise < 0) || iy == y

doPart1 :: Double -> Double -> [Hailstone] -> Int
doPart1 mn mx hs = length res where
    pairs = choose hs 2
    intercepts pair = 
        let h1 = pair !! 0
            h2 = pair !! 1 in 
                ((h1, h2), (findIntercept h1 h2))
    allIntercepts = fmap intercepts pairs
    res = filter (\((h1,h2),i) -> inBounds mn mx h1 h2 i) allIntercepts

-- alebra got me here!
-- x+tdx = X+tDX
-- t = (X-x)/(dx-DX) = (Y-y)/(dy-DY)
-- (X-x)(dy-DY) = (Y-y)(dx-DX)
-- YDX-XDY = Ydx - ydx + yDX + xdy + xDY - Xdy 
-- X(dy2 - dy1) + DX(y1-y2) + Y(dx-dx2) + DY(x1-x2) = x2dy2 - y2dx2 - x1dy1 + y1dx1
coeffsXy :: Hailstone -> Hailstone -> ([Double], Double)
coeffsXy ((x1, y1, _), (dx1, dy1, _)) ((x2, y2, _), (dx2, dy2, _)) = 
    ([(dy2-dy1), (y1-y2), (dx1-dx2), (x2-x1)], (x2*dy2 - y2*dx2 - x1*dy1 + y1*dx1))

coeffsXz :: Hailstone -> Hailstone -> ([Double], Double)
coeffsXz ((x1, _, z1), (dx1, _, dz1)) ((x2, _, z2), (dx2, _, dz2)) = 
    ([(dz2-dz1), (z1-z2), (dx1-dx2), (x2-x1)], (x2*dz2 - z2*dx2 - x1*dz1 + z1*dx1))

doPart2 :: [Hailstone] -> IO Double
doPart2 hs = res where
    pairs = choose hs 2
    -- 4 equations for 4 unknowns x, dx, y, dy
    xy = fmap (\p -> coeffsXy (p !! 0) (p !! 1)) (take 3 pairs)
    -- same for x, dx, z, dz
    xz = fmap (\p -> coeffsXz (p !! 0) (p !! 1)) (take 3 pairs)
    process a = foldr (\(curCoeff, curResult) (coeffRes, resultRes) -> ((fmap toRational curCoeff):coeffRes, (toRational curResult):resultRes)) ([], []) a
    solve = let (coeffXy, resXy) = process xy
                (coeffXz, resXz) = process xz 
                coeff = (fmap (\a -> a ++ [0,0]) coeffXy) ++ (fmap (\a -> (take 2 a) ++ [0,0] ++ (take 2 (drop 2 a))) coeffXz) 
                result = resXy ++ resXz in 
                    solveRationalLinearEqsAll Z3 100 coeff result
    res = fmap (\a -> sum . concat . fmap (fmap fromRational) $ a) solve

day24 :: Bool -> String -> (Int, Int)
day24 test input = (part1, part2) where
    hailstones = fmap parseHailstone (lines input)
    part1 = doPart1 (if test then 7 else 200000000000000) (if test then 27 else 400000000000000) hailstones
    part2 = round . unsafePerformIO $ doPart2 hailstones