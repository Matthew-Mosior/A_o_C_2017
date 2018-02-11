--AoC 2017 Day 2 Part 1
module Main where

--imports
import Data.List
import Data.Char

--type definitions and corresponding function definitions.

--chop
chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as

--divvy
divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m lst = filter (\ws -> (n == length ws)) choppedl
  where choppedl = chop (\xs -> (take n xs , drop m xs)) lst

--mapNested
mapNested :: (a -> b) -> [[a]] -> [[b]]
mapNested = map . map

--minoflists
minoflists :: String -> [Int]
minoflists input = map (minimum) (divvy 16 16 (map read (words input)))

--maxoflists
maxoflists :: String -> [Int]
maxoflists input = map (maximum) (divvy 16 16 (map read (words input)))

--answer
answer :: String -> String -> Int
answer maxinputs mininputs = sum (zipWith (-) ((maxoflists maxinputs)) (minoflists mininputs))

--Main function.
main :: IO ()
main = do
    inputfile <- readFile "AoCDay2.txt"
    let answerofinput = answer inputfile inputfile
    print (answerofinput)
