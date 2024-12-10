module Day_09 where

import Data.Char (digitToInt)

spaceLayout :: String -> String
spaceLayout s = 
    let 
        go :: Int -> String -> String 
        go _ []       = []
        go i [x]      = concat $ replicate (digitToInt x) (show i)
        go i (x:y:xs) = (concat $ replicate (digitToInt x) (show i)) ++ (replicate (digitToInt y) '.') ++ go (i+1) xs
    in go 0 s

compactLayout :: String -> String
compactLayout s =
    let 
        l = length s
        go :: String -> Int -> String -> Int -> String
        go xs@(x:xr) xi ys@(y:yr) yi
            | xi + yi == l = []
            | x /= '.'     = x : go xr (xi+1) ys yi
            | y == '.'     = go xs xi yr (yi+1)
            | otherwise    = y : go xr (xi+1) yr (yi+1)
        go [] _ _  _ = []
        go _  _ [] _ = []
    in go s 0 (reverse s) 0
            
checksum :: String -> Int
checksum = sum . zipWith (*) [0..] . map digitToInt

day_09 :: String -> (Integer, Integer)
day_09 = \_ -> (-1, -1)
