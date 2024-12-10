module Day_09 where

import Data.Char (digitToInt)
import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Block = Full {size :: Int, number :: Int} | Empty {size :: Int} 
    deriving (Eq)

instance Show Block where
    show (Full s n) = show (replicate s n)
    show (Empty s) = concat $ replicate s " ."

isEmptyBlock :: Block -> Bool
isEmptyBlock (Empty _) = True
isEmptyBlock _         = False

blockToList :: Block -> [Int]
blockToList (Full s n) = replicate s n
blockToList (Empty s) = replicate s 0

blockLayout :: String -> [Block]
blockLayout s = 
    let 
        go :: Int -> String -> [Block] 
        go _ []       = []
        go i [x]      = [Full (digitToInt x) i]
        go i (x:y:xs) = Full (digitToInt x) i : Empty (digitToInt y) : go (i+1) xs
    in go 0 s

showBlockLayout :: [Block] -> String
showBlockLayout = concat . map show

compactBlocks :: [Block] -> [Block]
compactBlocks bs =
    Vec.toList $
    snd $
    until 
    (\(i, _) -> i == 0)
    (\(i, v) -> (i - 1, fitBlock i v))
    (length bs - 1, Vec.fromList bs)

fitBlock :: Int -> Vector Block -> Vector Block
fitBlock i v = 
    case (v Vec.! i) of 
        Empty _  -> v
        Full s _ -> swapIndex v (until (\ix -> ix >= i || found ix s) (+ 1) 0) i
    where 
        found ind sz = (isEmptyBlock $ v Vec.! ind) && (size (v Vec.! ind)) >= sz


swapIndex :: Vector Block -> Int -> Int -> Vector Block
swapIndex v l h
    | l >= h = v
swapIndex v l h = 
    let a  = v Vec.! l
        bf = v Vec.! h
        be = Vec.singleton $ Empty (size a - size bf)
    in
        (\(x, y) -> if length be > 0 then x Vec.++ be Vec.++ y else x Vec.++ y) $ 
        Vec.splitAt (l+1) (v Vec.// [(l, bf), (h, Empty (size bf))])

compactLayout :: [Int] -> [Int]
compactLayout s =
    let 
        l = length s
        go :: [Int] -> Int -> [Int] -> Int -> [Int]
        go xs@(x:xr) xi ys@(y:yr) yi
            | xi + yi == l = []
            | x /= -1      = x : go xr (xi+1) ys yi
            | y == -1      =     go xs  xi    yr (yi+1)
            | otherwise    = y : go xr (xi+1) yr (yi+1)
        go [] _ _  _ = []
        go _  _ [] _ = []
    in go s 0 (reverse s) 0
            
checksumBlock :: [Block] -> Int 
checksumBlock = sum . zipWith (*) [0..] . concat . map blockToList

spaceLayout :: String -> [Int]
spaceLayout s = 
    let 
        go :: Int -> String -> [Int] 
        go _ []       = []
        go i [x]      = replicate (digitToInt x) i
        go i (x:y:xs) = replicate (digitToInt x) i ++ (replicate (digitToInt y) (-1)) ++ go (i+1) xs
    in go 0 s

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..] 

day_09 :: String -> (Integer, Integer)
day_09 s = (fromIntegral $ checksum $ compactLayout $ spaceLayout s', 
            fromIntegral $ checksumBlock $ compactBlocks $ blockLayout s')
    where s' = init s
