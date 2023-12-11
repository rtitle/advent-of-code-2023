import Data.Maybe
import Test.Hspec

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Day10 (day10)
import Day11 (day11)

main :: IO ()
main = hspec $ do
    _testDay 1 day1 ("day1-p1.txt", Just "day1-p2.txt") (142, 281)
    testDay 2 day2 (8, 2286)
    testDay 3 day3 (4361, 467835)
    testDay 4 day4 (13, 30)
    testDay 5 day5 (35, 46)
    testDay 6 day6 (288, 71503)
    testDay 7 day7 (6440, 5905)
    _testDay 8 day8 ("day8-p1.txt", Just "day8-p2.txt") (6, 6)
    testDay 9 day9 (114, 2)
    _testDay 10 day10 ("day10-p1.txt", Just "day10-p2.txt") (8, 10)
    testDay 11 (day11 True) (374, 8410)

testDay :: Int -> (String -> (Int, Int)) -> (Int, Int) -> Spec
testDay n day (p1, p2) = _testDay n day ("day" ++ (show n) ++ ".txt", Nothing) (p1, p2)

_testDay :: Int -> (String -> (Int, Int)) -> (String, Maybe String) -> (Int, Int) -> Spec
_testDay n day (in1, in2) (p1, p2) = describe ("Day" ++ show n) $ do
    input1 <- runIO (readFile ("test/data/" ++ in1))
    input2 <- runIO . traverse (\f -> readFile ("test/data/" ++ f)) $ in2
    let (part1, part2) = day input1
    let part2' = fromMaybe part2 . fmap (snd . day) $ input2
    it "part 1" $ do
        part1 `shouldBe` p1
    it "part 2" $ do
        part2' `shouldBe` p2