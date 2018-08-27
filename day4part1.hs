--AoC 2017 Day 2 Part 1

--imports
import Data.List
import Data.Char

--type definitions and corresponding function definitions.

--linefeed
linefeed :: String -> [[String]]
linefeed xs = map words (lines xs)

--grouper
grouper :: String -> [[[String]]]
grouper [] = [[[]]]
grouper xs = map (group) ((map (sort) (linefeed xs)))

--answer
answer :: String -> Int
answer [] = 0
answer xs = length (snd (partition (any (\x -> length x > 1)) (grouper xs)))

--Main function.
main :: IO ()
main = do
    inputfile <- readFile "/run/media/mmosior/Data/Data/Haskell/AdventOfCode/2017/puzzle_input/day4input.txt"
    let answerofinput = answer inputfile 
    print (answerofinput)

