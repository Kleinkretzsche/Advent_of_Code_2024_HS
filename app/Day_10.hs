module Day_10 where

import Util.Field (Field)
import Data.Set (Set)

import qualified Util.Field as F
import qualified Data.Set as S


type TopMap = Field Int

getInput :: String -> TopMap
getInput s = (\x -> read [x]) <$> F.fromString s

startingPositions :: TopMap -> [(Int ,Int)] 
startingPositions = F.indexWhere (== 0)

getAround :: (Int, Int) -> [(Int, Int)]
getAround i = [i .+. (-1, 0), i .+. (0, -1), i .+. (1, 0), i .+. (0,1)]

branchTillTop :: TopMap -> (Int, Int) -> Integer
branchTillTop t i = 
    let 
        go :: (Int, Int) -> Int -> Set (Int, Int)
        go i' 9 = S.singleton i'
        go i' h = S.unions $ 
                 (\x -> go x (h+1)) <$>
                 filter (\x -> F.lookup x t == Just (h+1)) 
                 (getAround i')
    in fromIntegral $ S.size $ go i 0

countAllWays :: TopMap -> (Int, Int) -> Integer
countAllWays t i = 
    let 
        go :: (Int, Int) -> Int -> Int
        go _ 9 = 1
        go i' h = sum $ (\x -> go x (h+1)) <$>
                        filter (\x -> F.lookup x t == Just (h+1))
                        (getAround i')
    in fromIntegral $ go i 0

(.+.) :: (Int, Int) -> (Int ,Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

day_10_a :: TopMap -> Integer
day_10_a t = sum $ (branchTillTop t) <$> startingPositions t

day_10_b :: TopMap -> Integer
day_10_b t = sum $ (countAllWays t) <$> startingPositions t

day_10 :: String -> (Integer, Integer)
day_10 s = (day_10_a input, -1)
    where input = getInput s
