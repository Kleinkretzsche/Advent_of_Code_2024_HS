module Day_09 where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Buffer a = MkBuffer
    { buf   :: Vector a 
    , start :: Int
    , end   :: Int
    } 
    deriving Show

bufFromVec :: Vector a -> Buffer a
bufFromVec v = MkBuffer v 0 (Vec.length v - 1)

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

advanceBuf :: Buffer Char -> Buffer Char
advanceBuf b = 
    let 
        b'  = until (\(MkBuffer x y z) -> y >= z || (x Vec.! y) == '.') 
                    (\(MkBuffer x y z) -> MkBuffer x (y+1) z) 
                    b
        b'' = until (\(MkBuffer x y z) -> z <= y || (x Vec.! z) /= '.') 
                    (\(MkBuffer x y z) -> MkBuffer x y (z - 1)) 
                    b'
    in 
        if (start b'' >= end b'')
        then b''
        else MkBuffer (swapIndex (buf b'') (start b'') (end b'')) (start b'' + 1) (end b'' - 1)

checksum :: Vector Char -> Integer
checksum v = sum $ zipWith (*) (toIntList v) [0..]
    where 
        toIntList :: Vector Char -> [Integer]
        toIntList v' = (\x -> read [x]) <$> Vec.toList v'

day_09_a :: Vector Char -> Integer
day_09_a v = checksum $ Vec.filter (/= '.') $ buf $ until (\(MkBuffer _ l h) -> l >= h) advanceBuf $ bufFromVec v

day_09 :: String -> (Integer, Integer)
day_09 s = (day_09_a v, -1)
    where v = prepareInput s
