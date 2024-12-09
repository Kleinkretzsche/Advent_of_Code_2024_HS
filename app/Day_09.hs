module Day_09 where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

swapIndex :: Vector a -> Int -> Int -> Vector a 
swapIndex v a b = 
    let 
        la = v Vec.! a
        lb = v Vec.! b
    in v Vec.// [(a, lb), (b, la)]

prepareInput :: String -> Vector Char
prepareInput s = go s True 0
    where 
        go :: String -> Bool -> Integer -> Vector Char
        go [] _ _         = Vec.empty
        go ['\n'] _ _     = Vec.empty
        go (x:xs) True  i = (Vec.concat $ replicate (read [x]) (Vec.fromList $ show i)) Vec.++ go xs False (i+1)
        go (x:xs) False i = Vec.replicate (read [x]) ('.') Vec.++ go xs True i

efficientSwap :: Vector Char -> Vector Char
efficientSwap vec = 
    fst $ 
    until 
        (\(_, (l, h)) -> l > h) 
        (\(v, (l, h)) -> (swapIndex v l h, (stepUp l, stepDown h)))
        (vec, (1, (length vec) - 2))
    where 
        stepUp   i = until (\x -> vec Vec.! x == '.') (+ 1) (i+1)
        stepDown i = until (\x -> vec Vec.! x /= '.') (\x -> x - 1) (i-1)

checksum :: Vector Char -> Integer
checksum v = sum $ zipWith (*) (toIntList v) [0..]
    where 
        toIntList :: Vector Char -> [Integer]
        toIntList v' = (\x -> read [x]) <$> Vec.toList v'

day_09_a :: Vector Char -> Integer
day_09_a = checksum . Vec.filter (/= '.') . efficientSwap

day_09 :: String -> (Integer, Integer)
day_09 = undefined
