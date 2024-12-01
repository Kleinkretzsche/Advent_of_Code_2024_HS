module Day_01 where

import Data.List
import Data.List.Utils
import Data.List.Split

readInt :: String -> Int
readInt = read

getInput :: String -> [[String]]
getInput = transpose . map (\x -> filter (/= []) $ splitOn [' '] x) . splitOn ['\n']

day01_a :: [[String]] -> [Int]
day01_a [as, bs] = map (\(x, y) -> abs (x - y)) $ 
                   zip (sort $ map readInt as) (sort $ map readInt bs)
day01_a _        = error "incorrect input"

day01_b :: [[String]] -> [Int]
day01_b [as, bs] = map (\x -> (countElem x bs) * (read x)) as
day01_b _        = error "incorrect input"

day_01 :: IO ()
day_01 = do
    s <- readFile "data/01.in" 
    putStrLn $ show $ sum $ day01_a $ getInput s
    putStrLn $ show $ sum $ day01_b $ getInput s
