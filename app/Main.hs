module Main where

import Data.List
import GHC.Utils.Misc

readInt :: String -> Int
readInt = read

getInput :: String -> [[String]]
getInput = transpose . map (\x -> filter (/= []) $ split ' ' x) . split '\n'

day01_a :: [[String]] -> [Int]
day01_a [as, bs] = map (\(x, y) -> abs (x - y)) $ 
                   zip (sort $ map readInt as) (sort $ map readInt bs)
day01_a _        = error "incorrect input"

day01_b :: [[String]] -> [Int]
day01_b [as, bs] = map (\x -> (count (==x) bs) * (read x)) as
day01_b _        = error "incorrect input"

main :: IO ()
main = do
    s <- readFile "data/01.in" 
    putStrLn $ show $ sum $ day01_a $ getInput s
