module Day_01 where

import Data.List
import Data.List.Utils
import Data.List.Split

getInput :: String -> [[String]]
getInput = transpose . map (\x -> filter (/= []) $ splitOn [' '] x) . splitOn ['\n']

readInt :: String -> Int
readInt = read

day_01_a :: [[String]] -> Int
day_01_a [as, bs] = sum $ map (\(x, y) -> abs (x - y)) $ 
                   zip (sort $ map readInt as) (sort $ map readInt bs)
day_01_a _        = error "incorrect input"

day_01_b :: [[String]] -> Int
day_01_b [as, bs] = sum $ map (\x -> (countElem x bs) * (read x)) as
day_01_b _        = error "incorrect input"

day_01 :: String -> (Int, Int)
day_01 s = (day_01_a input, day_01_b input)
    where input = getInput s
