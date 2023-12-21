module Day19 (day19) where

import Control.Monad (void)
import Data.Either
import Data.List (sort, nub)
import Data.List.Split (splitWhen)
import Text.Parsec 
import Text.Parsec.String (Parser)
import qualified Data.Map as M

type Workflows = M.Map String [Rule]
data Target = Tgt String | A | R deriving (Eq, Show)
data Op = Less | Greater | Equal | LessEqual | GreaterEqual | NotEqual deriving (Eq, Show)
data Condition = Condition Char Op Int deriving (Show)

data Rule = Rule {
    target :: Target,
    cond :: Maybe Condition
} deriving (Show)

data Part = Part {
    name :: Char,
    value :: Int
} deriving (Show)

data Expr = Primitive Condition | And Expr Expr | Or Expr Expr | Not Expr deriving (Show)

parseOp :: Char -> Op
parseOp '<' = Less
parseOp '>' = Greater
parseOp _ = Equal

parseTarget :: String -> Target
parseTarget "A" = A
parseTarget "R" = R
parseTarget s = Tgt s

-- px{a<2006:qkq,m>2090:A,rfg}
parseWorkflow :: String -> Workflows
parseWorkflow s = fromRight M.empty $ parse p "" s where
    p = do 
        n <- many1 lower
        rs <- rules
        return $ M.singleton n rs
    rules = between (char '{') (char '}') (rule `sepBy` (char ','))
    rule = do
        c <- letter
        (condition c) <|> (direct c)
    condition :: Char -> Parser Rule
    condition c = do
        op <- operator
        n <- many1 digit
        void $ char ':'
        t <- many1 letter
        return $ Rule (parseTarget t) (Just (Condition c (parseOp op) (read n)))
    direct :: Char -> Parser Rule
    direct c = do 
        r <- (many letter)
        return $ Rule (parseTarget (c:r)) Nothing
    operator = (char '<') <|> (char '>') <|> (char '=')

-- {x=2127,m=1623,a=2188,s=1013}
parseParts :: String -> [Part]
parseParts s = fromRight [] $ parse p "" s where
    p = between (char '{') (char ('}')) (def `sepBy` (char ','))
    def = do
        c <- lower
        void $ char '='
        n <- many1 digit
        return $ Part c (read n)

applyRule :: Rule -> [Part] -> Maybe Target
applyRule (Rule t Nothing) _ = Just t
applyRule (Rule t (Just (Condition c o n))) ps
  | o == Less && part < n = Just t
  | o == Greater && part > n = Just t
  | o == Equal && part == n = Just t
  | otherwise = Nothing
  where part = value . head . filter ((==c) . name) $ ps

runPart :: Workflows -> [Part] -> Bool
runPart w p = inner "in" 0 where
    rules a = w M.! a
    inner cur n 
      | n >= (length (rules cur)) = False
      | otherwise = case (applyRule ((rules cur) !! n) p) of
        Just A -> True
        Just R -> False
        Just (Tgt s) -> inner s 0
        Nothing -> inner cur (n+1)

doPart1 :: Workflows -> [[Part]] -> Int
doPart1 w ps = sum . fmap value . concat . filter (runPart w) $ ps 

cond2Range :: Condition -> M.Map Char [(Int, Int)]
cond2Range (Condition c Less n) = M.insert c [(1,n-1)] fullMap
cond2Range (Condition c Greater n) = M.insert c [(n+1,4000)] fullMap 
cond2Range (Condition c LessEqual n) = M.insert c [(1,n)] fullMap
cond2Range (Condition c GreaterEqual n) = M.insert c [(n,4000)] fullMap
cond2Range (Condition c Equal n) = M.insert c [(n,n)] fullMap
cond2Range (Condition c NotEqual n) = M.insert c [(1,n-1), (n+1,4000)] fullMap

invert :: Condition -> Condition
invert (Condition c Greater n) = Condition c LessEqual n
invert (Condition c Less n) = Condition c GreaterEqual n
invert (Condition c Equal n) = Condition c NotEqual n
invert (Condition c NotEqual n) = Condition c Equal n
invert (Condition c GreaterEqual n) = Condition c Less n
invert (Condition c LessEqual n) = Condition c Greater n

fullMap :: M.Map Char [(Int, Int)]
fullMap = M.fromList . fmap (\c -> (c, [(1,4000)])) $ ['a'..'z']

mergeAnd :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeAnd _ [] = []
mergeAnd [] _ = []
mergeAnd ((a1,a2):as) ((b1,b2):bs) = if rangeOverlap (a1,a2) (b1,b2) 
    then 
        if a2 > b2
            then (max a1 b1, b2) : mergeAnd ((a1,a2):as) bs
            else (max a1 b1, a2) : mergeAnd as ((b1,b2):bs)
    else if a1 < b1
        then mergeAnd as ((b1,b2):bs) 
        else mergeAnd ((a1,a2):as) bs

rangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlap (a1,a2) (b1,b2) = b2 >= a1 && b1 <= a2
    
computePart2 :: M.Map Char [(Int, Int)] -> Int
computePart2 = product . fmap (inner . snd) . filter (\(k,_) -> k `elem` "xmas") . M.toList where
    inner = sum . fmap (\(a,b) -> b - a + 1)

doPart2 :: Workflows -> Int
doPart2 w = inner "in" 0 M.empty where
    inner :: String -> Int -> M.Map Char [(Int, Int)] -> Int
    inner workflow index m = 
        let rules = w M.! workflow
            curRule = rules !! index in
            case (curRule) of 
                (Rule A Nothing) -> computePart2 m 
                (Rule R Nothing) -> 0
                (Rule (Tgt t) Nothing) -> inner t 0 m
                (Rule A (Just c)) -> (computePart2 (M.unionWith (\a b -> nub . sort $ mergeAnd a b) m (cond2Range c))) + (if (length rules)-1 == index then 0 else inner workflow (index+1) (M.unionWith (\a b -> nub . sort $ mergeAnd a b) m (cond2Range (invert c))))
                (Rule R (Just c)) -> (if (length rules)-1 == index then 0 else inner workflow (index+1)  (M.unionWith (\a b -> nub . sort $ mergeAnd a b) m (cond2Range (invert c))))
                (Rule (Tgt t) (Just c)) -> inner t 0 (M.unionWith (\a b -> nub . sort $ mergeAnd a b) m (cond2Range c)) + (if (length rules)-1 == index then 0 else inner workflow (index+1)  (M.unionWith (\a b -> nub . sort $ mergeAnd a b) m (cond2Range (invert c))))

day19 :: String -> (Int, Int)
day19 input = (part1, part2) where
    ls = splitWhen null (lines input)
    workflows = M.unions . fmap parseWorkflow . head $ ls
    parts = fmap parseParts . head . tail $ ls
    part1 = doPart1 workflows parts
    part2 = doPart2 workflows
