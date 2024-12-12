module Day_11 where

import Data.Monoid

-- https://stackoverflow.com/questions/3208258/memoization-in-haskell

data Tree v = Tree (Tree v) v (Tree v) deriving Show

memo1 :: (Integer -> t) -> Integer -> t
memo1 f = index nats
  where
    nats = go 0 1
    go i s = Tree (go (i + s) s') (f i) (go (i + s') s')
      where
        s' = 2 * s
    index (Tree l v r) i
        | i < 0 = f i
        | i == 0 = v
        | otherwise = case (i - 1) `divMod` 2 of
            (i', 0) -> index l i'
            (i', _) -> index r i'

memo2 :: (Integer -> Integer -> t) -> (Integer -> Integer -> t)
memo2 f = memo1 (memo1 . f)

blink :: Integer -> Integer -> Sum Integer
blink = memo2 blink'
  where
    blink' :: Integer -> Integer -> Sum Integer
    blink' c n
        | c == 0 = 1
        | n == 0 = blink c' 1
        | even digits = blink c' l <> blink c' r
        | otherwise = blink c' $ n * 2024
      where
        digits :: Integer
        digits = succ . floor . logBase 10 . fromIntegral $ n
        (l, r) = n `divMod` (10 ^ (digits `div` 2))
        c' = pred c

doBlinks :: Integer -> [Integer] -> Integer
doBlinks n = getSum . mconcat . fmap (blink n)

day_11_a :: [Integer] -> Integer
day_11_a = 

day_11_b :: [Integer] -> Integer
day_11_b = doBlinks 75

day_11 :: String -> (Integer, Integer)
day_11 s = (day_11_a input, day_11_b input)
    where input = map (read) $ words s

