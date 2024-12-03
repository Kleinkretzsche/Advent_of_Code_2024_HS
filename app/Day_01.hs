module Day_01 where

import Data.List
import Data.List.Utils
import Data.List.Split

getInput :: String -> [[String]]
getInput = transpose . map (\x -> filter (/= []) $ splitOn [' '] x) . splitOn ['\n']

readInteger :: String -> Integer
readInteger = read

day_01_a :: [[String]] -> Integer
day_01_a [as, bs] = sum $ map (\(x, y) -> abs (x - y)) $ 
                   zip (sort $ map readInteger as) (sort $ map readInteger bs)
day_01_a _        = error "incorrect input"

day_01_b :: [[String]] -> Integer
day_01_b [as, bs] = sum $ map (\x -> (fromIntegral $ countElem x bs) * (read x)) as
day_01_b _        = error "incorrect input"

day_01 :: String -> (Integer, Integer)
day_01 s = (day_01_a input, day_01_b input)
    where input = getInput s
