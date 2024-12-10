module Day_09 where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

prepareInput :: String -> Vector Char
prepareInput s = go s True 0
    where 
        go :: String -> Bool -> Integer -> Vector Char
        go [] _ _         = Vec.empty
        go ['\n'] _ _     = Vec.empty
        go (x:xs) True  i = (Vec.concat $ replicate (read [x]) (Vec.fromList $ show i)) Vec.++ go xs False (i+1)
        go (x:xs) False i = Vec.replicate (read [x]) ('.') Vec.++ go xs True i

swapIndex :: Vector a -> Int -> Int -> Vector a 
swapIndex v a b = 
    let 
        la = v Vec.! a
        lb = v Vec.! b
    in v Vec.// [(a, lb), (b, la)]

efficientSwap :: Vector Char -> Vector Char
efficientSwap vec = 
    fst $ 
    until 
        (\(_, (l, h)) -> l > h) 
        update
        (start , (0, (length vec) - 1))
    where 
        start = swapIndex vec 0 ((length vec) - 1)

        update :: (Vector Char, (Int, Int)) -> (Vector Char, (Int, Int))

        update (v', (l, h)) = 
            let v'' = swapIndex v' l h
            in (v'', (stepUp v'' l, stepDown v'' h))

        stepUp   v' i = until (\x -> v' Vec.! x == '.') (+ 1) i
        stepDown v' i = until (\x -> v' Vec.! x /= '.') (\x -> x - 1) i

checksum :: Vector Char -> Integer
checksum v = sum $ zipWith (*) (toIntList v) [0..]
    where 
        toIntList :: Vector Char -> [Integer]
        toIntList v' = (\x -> read [x]) <$> Vec.toList v'

day_09_a :: Vector Char -> Integer
day_09_a = checksum . Vec.takeWhile (/= '.') . efficientSwap

day_09 :: String -> (Integer, Integer)
day_09 s = (day_09_a v, -1)
    where v = prepareInput s
