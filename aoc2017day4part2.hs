--AoC 2017 Day 4 Part 2

--imports
import Data.List
import Data.Char

{-type definitions and corresponding function definitions.-}

{-General Utility Functions.-}

--same
same :: (Eq a) => [a] -> [a] -> Bool
same xs ys = null (xs \\ ys) && null (ys \\ xs)

--singlenest
singlenest :: [a]-> [[a]]
singlenest [] = []
singlenest xs = [xs]

{----------------------------}

--linefeed
linefeed :: String -> [[String]]
linefeed xs = map words (lines xs)

--allpairs
allpairs :: [String] -> [(String,String)]
allpairs [] = []
allpairs xs = [(x,y) | (x:ys) <- tails xs , y <- ys]

--mappedallpairs
mappedallpairs :: String -> [[(String,String)]]
mappedallpairs [] = [[]]
mappedallpairs xs = map (allpairs) (linefeed xs)

--anagramfilter
anagramfilter :: [[(String,String)]] -> [[(String,String)]]
anagramfilter [] = []
anagramfilter (x:xs) = (singlenest $ (filter (\y -> same (fst y) (snd y)) x)) ++ (anagramfilter xs)

--answer
answer :: [[String]] -> [[(String,String)]] -> Int
answer [] [] = 0
answer xs [] = 0
answer [] ys = 0 
answer xs ys = (length xs) - (length ys)

{----------------------------------------------------------}

--Main function.
main :: IO ()
main = do
    inputfile <- readFile "day4input.txt"
    let answerofinput = answer (linefeed inputfile) (anagramfilter (mappedallpairs inputfile))
    print (answerofinput)

