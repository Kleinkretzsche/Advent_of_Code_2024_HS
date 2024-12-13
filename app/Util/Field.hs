module Util.Field where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

data Field t = MkField 
    { table :: M.Map (Int,Int) t
    , dim :: (Int, Int)
    }
    deriving Eq

instance Show t => Show (Field t) where
    show (MkField m d) = 
        case (sequence (sequence <$> [[(M.lookup (y, x) m) | x <- [1..snd d]]| y <- [1..fst d]])) of 
            Just xs -> "\n" ++ (unlines $ fmap unwords $ fmap (show <$>) xs)
            Nothing -> ""

instance (Functor Field) where
    fmap f (MkField m d) = MkField (M.fromList $ (\(i, x) -> (i, f x)) <$> M.toList m) d

lookup :: (Int, Int) -> Field a -> Maybe a
lookup i (MkField m _) = M.lookup i m

lookupUnsafe :: (Int, Int) -> Field a -> a
lookupUnsafe i f = fromJust $ lookup i f 

find :: Field a -> (Int, Int) -> Maybe a
find (MkField m _) i = M.lookup i m

null :: Field a -> Bool
null (MkField m _) = M.null m

indexWhere :: (a -> Bool) -> Field a -> [(Int, Int)]
indexWhere f (MkField m d) = filter pr [(a,b) | a <- [0..snd d], b <- [0..snd d]]
    where 
        pr x = 
            case (x `M.lookup` m) of 
                Just el -> f el
                Nothing -> False

validIndex :: (Int, Int) -> Field a -> Bool
validIndex (y, x) (MkField _ (my, mx)) = (y > 0 && y <= my && x > 0 && x <= mx)

fromList :: [[a]] -> Field a
fromList xs = MkField (M.fromList $ zip [(a,b) | a <- [1..y], b <-[1..x]] (concat xs)) (y, x)
    where y  = length  xs
          x  = length (xs !! 0)

fromString :: String -> Field Char
fromString s = fromList $ concat $ words <$> lines s

toList :: Field a -> [((Int, Int), a)]
toList (MkField m _) = M.toList m
