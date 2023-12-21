module Day20 (test) where

import Control.Monad (void)
import Control.Monad.RWS (RWS, evalRWS, get, put, tell)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Text.Parsec 
-- import Text.Parsec.String (Parser)
import qualified Data.Map as M

data Pulse = High | Low deriving (Eq,Show)
data Module = Broadcaster [String] | FlipFlop Bool [String] | Conjunction (M.Map String Pulse) [String] | Button deriving (Show)
type Modules = M.Map String Module

parseModule :: String -> M.Map String Module
parseModule s = fromRight M.empty $ parse p "" s where
    p = flipflop <|> broadcaster <|> conjunction
    broadcaster = do
         void $ string "broadcaster -> "
         tgts <- targets
         return $ M.singleton "broadcaster" (Broadcaster tgts)
    flipflop = do
        void $ char '%'
        n <- name
        void $ string " -> "
        tgts <- targets
        return $ M.singleton n (FlipFlop False tgts)
    conjunction = do
        void $ char '&'
        n <- name
        void $ string " -> "
        tgts <- targets
        return $ M.singleton n (Conjunction M.empty tgts)
    name = many lower
    targets = name `sepBy` (string ", ")

sendPulse :: Modules -> String -> String -> Pulse -> ([(Pulse, String, String)], Modules)
sendPulse ms n input p = fromMaybe ([], ms) . fmap inner $ M.lookup n ms where
    inner (Broadcaster ts) = (fmap (\t -> (Low,n,t)) ts, ms)
    inner (FlipFlop b ts)
      | p == High = ([], ms)
      | b == False = (fmap (\t -> (High,n,t)) ts, M.insert n (FlipFlop True ts) ms)
      | otherwise = (fmap (\t -> (Low,n,t)) ts, M.insert n (FlipFlop False ts) ms)
    inner (Conjunction state ts) = let updatedState = M.insert input p state in
        if (all (==High) . fmap snd . M.toList $ updatedState)
            then (fmap (\t -> (Low,n,t)) ts, M.insert n (Conjunction updatedState ts) ms)
            else (fmap (\t -> (High,n,t)) ts, M.insert n (Conjunction updatedState ts) ms)
    inner _ = ([], ms)

initialConjunctionState :: Modules -> Modules
initialConjunctionState m = updated where
    updated = M.fromList . fmap (\(k,v) -> (k, updateConj k v)) . M.toList $ m
    updateConj n (Conjunction _ ts) = Conjunction (M.fromList . fmap (\a -> (a, Low)) $ process M.! n) ts
    updateConj _ m' = m'
    process = foldr inner M.empty (M.toList m)
    inner (k,v) r = M.unionWith (++) r (M.fromList (fmap (\c -> (c,[k])) (conjunctionTargets v)))
    conjunctionTargets (Conjunction _ ts) = filter (\t -> t `elem` allConjuctionNames) ts
    conjunctionTargets (Broadcaster ts) = filter (\t -> t `elem` allConjuctionNames) ts
    conjunctionTargets (FlipFlop _ ts) = filter (\t -> t `elem` allConjuctionNames) ts
    conjunctionTargets _ = []
    allConjuctionNames = fmap fst . filter (\(_,v) -> isConjunction v) $ (M.toList m) 
    isConjunction (Conjunction _ _) = True
    isConjunction _ = False

simulate :: Modules -> Int
simulate ms = total . snd $ evalRWS (inner 0) () ([(Low, "button", "broadcaster")], ms) where
    inner :: Int -> RWS () [(Int, Int)] ([(Pulse, String, String)], Modules) ()
    inner 1000 = return ()
    inner n = do
        (q, curModules) <- get
        case (q) of 
            [] -> do
                put ([(Low, "button", "broadcaster")], curModules)
                inner (n+1) 
            ((p,prev,s):xs) -> do
                if p == Low then tell [(1,0)] else tell [(0,1)]
                let (nextPulses, nextModules) = sendPulse curModules s prev p
                put (xs ++ nextPulses, nextModules)
                inner n

total :: [(Int, Int)] -> Int
total xs = a * b where
    a = sumFst xs
    b = sumSnd xs
    sumFst = sum . fmap fst
    sumSnd = sum . fmap snd

test :: IO ()
test = do
    input <- readFile "data/day20.txt"
    let modules = initialConjunctionState . M.unions . fmap parseModule $ (lines input)
    let part1 = simulate modules
    print part1