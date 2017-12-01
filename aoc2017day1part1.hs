--AoC 2017 Day 1 Part 1
module Main where

--type definitions and corresponding function definitions.

--convert
convert :: String -> [Int]
convert input = map(read.(:"")) input

--pairs
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

unpair :: [(a,a)] -> [a]
unpair [] = []
unpair ((x1,x2):xs) = x1:x2:unpair xs

--answer
answer :: String -> Int
answer input = quot (sum(unpair(filter(uncurry(==)) (pairs(convert(input)))))) 2

main :: IO ()
main = putStrLn ("This Haskell script will return the answer.")
