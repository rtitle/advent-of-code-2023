module Utils (runDay, str2Int) where

-- TODO: read and cache input from site?
getInput :: Int -> IO String
getInput n = readFile $ "./data/day" ++ (show n) ++ ".txt"

runDay :: Int -> (String -> (Int, Int)) -> IO ()
runDay n day = do
    putStrLn $ "*** Day " ++ (show n) ++ " ***"
    input <- getInput n
    let (part1, part2) = day input
    putStrLn $ "Part 1: " ++ (show part1)
    putStrLn $ "Part 2: " ++ (show part2)

str2Int :: String -> Int
str2Int s = read s :: Int