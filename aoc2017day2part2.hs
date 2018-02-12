--AoC 2017 Day 2 Part 2
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

--allpairs
allpairs :: Eq a => [a] -> [(a,a)]
allpairs xs = [(x1,x2) | x1 <- xs, x2 <- xs, x1 /= x2]

--lstlstpairs
lstlstpairs :: String -> [[(Int,Int)]]
lstlstpairs input = map allpairs (divvy 16 16 (map read (words input)))

--evenlydivisible
evenlydivisible :: String -> [[(Int,Int)]]
evenlydivisible input = map (filter (uncurry (\x y -> x `mod` y == 0))) (lstlstpairs input)
 
--divoftuple
divoftuple :: String -> [[Int]]
divoftuple input = mapNested (uncurry (\x y -> x `div` y)) (evenlydivisible input)

--answer
answer :: String -> Int
answer input = sum (map sum (divoftuple input))

--Main function.
main :: IO ()
main = do
    inputfile <- readFile "AoCDay2.txt"
    let answerofinput = answer inputfile 
    print (answerofinput)
