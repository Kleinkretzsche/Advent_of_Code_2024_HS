module Util.Field2 where

import qualified Data.Map as M

data Field2 t = F2 (M.Map (Int,Int) t) (Int, Int) deriving(Eq)

instance Show t => Show (Field2 t) where
    show (F2 m dim) = 
        case (sequence (sequence <$> [[(M.lookup (y, x) m) | x <- [1..snd dim]]| y <- [1..fst dim]])) of 
            Just xs -> "\n" ++ (unlines $ fmap unwords $ fmap (show <$>) xs)
            Nothing -> ""

lookup :: (Int, Int) -> Field2 a -> Maybe a
lookup i (F2 m _) = M.lookup i m

null :: Field2 a -> Bool
null (F2 m _) = M.null m

find :: Field2 a -> (Int, Int) -> Maybe a
find (F2 m _) i = M.lookup i m

dim :: Field2 a -> (Int, Int)
dim (F2 _ dim) = dim

indexWhere :: (a -> Bool) -> Field2 a -> [(Int, Int)]
indexWhere f (F2 m dim) = Prelude.filter pr [(a,b) | a <- [0..snd dim], b <- [0..snd dim]]
    where 
        pr x = 
            case (x `M.lookup` m) of 
                Just el -> f el
                Nothing -> False

validIndex :: (Int, Int) -> Field2 a -> Bool
validIndex (y, x) (F2 _ (my, mx)) = (y > 0 && y <= my && x > 0 && x <= mx)

fromList :: [[a]] -> Field2 a
fromList xs = F2 (M.fromList $ zip [(a,b) | a <- [1..y], b <-[1..x]] (concat xs)) (y, x)
    where y  = length  xs
          x  = length (xs !! 0)

fromString :: String -> Field2 Char
fromString s = fromList $ concat $ words <$> lines s

toList :: Field2 a -> [((Int, Int), a)]
toList (F2 m _) = M.toList m
