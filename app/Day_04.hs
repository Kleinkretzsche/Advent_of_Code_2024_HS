module Day_04 where

import Data.Map as M
import Data.List as L

type Field = (M.Map (Int, Int) Char, (Int, Int))

lookupField :: Field -> (Int, Int) -> Maybe Char
lookupField (f, _) i = M.lookup i f

allDirections :: (Int, Int) -> [[(Int, Int)]]
allDirections (x, y) = [genx [1..3], genx [-1,-2,-3], geny [1..3], geny [-1,-2,-3], 
                        gend1 [1..3], gend1 [-1,-2,-3], gend2 [1..3], gend2 [-1,-2,-3]]
    where genx xs = [(x+dx,y)  | dx <- xs]
          geny ys = [(x,y+dy)  | dy <- ys]
          gend1 l = [(x+d,y+d) | d  <- l]
          gend2 l = [(x-d,y+d) | d  <- l]

findChars :: Field -> Char-> [(Int, Int)]
findChars (m, (maxy, maxx)) c = Prelude.filter (\i -> (Just c) == M.lookup i m) 
                                [(a,b) | a <- [0..maxy], b <- [0..maxx]]

isXmas :: Field -> [(Int, Int)] -> Bool
isXmas f xs = (== (Just "MAS")) $ sequence $ (lookupField f) <$> xs

day_04_a :: Field -> Int
day_04_a field = length $ 
                 concat $ 
                 fmap (Prelude.filter (== True)) 
                 (fmap (isXmas field)) <$> allDirections <$> (findChars field 'X')

cross :: (Int, Int) -> [(Int, Int)]
cross (x, y) = [(x-1, y-1), (x+1, y+1), (x-1, y+1), (x+1, y-1)]

validCross :: Maybe [Char] -> Bool
validCross (Just [a, b, c, d]) = (L.sort [a,b]) == "MS" && (L.sort [c,d] == "MS")
validCross _                   = False

isMAS :: Field -> [(Int, Int)] -> Bool
isMAS f xs = validCross $ 
             sequence $ 
             (lookupField f) <$> xs

day_04_b :: Field -> Int
day_04_b f = length $ Prelude.filter (== True) $ isMAS f <$> cross <$> findChars f 'A'

getInput :: String -> Field
getInput s = (M.fromList $ zip [(a,b) | a <- [0..y-1], b <-[0..x-1]] (concat ls), (y, x))
    where 
      ls = lines s 
      x  = length (ls !! 0)
      y  = length ls

day_04 :: String -> (Integer, Integer)
day_04 input = (fromIntegral $ day_04_a f, fromIntegral $ day_04_b f)
    where f = getInput input
