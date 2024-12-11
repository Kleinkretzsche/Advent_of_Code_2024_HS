module Day_11 where

readInput :: String -> [Integer]
readInput = map read . words

blinkOnce :: [Integer] -> [Integer]
blinkOnce [] = []
blinkOnce (x:xs)
    | x == 0 = (1:blinkOnce xs)
    | len `mod` 2 == 0 = (read a:read b:blinkOnce xs)
        where 
            str = show x
            len = length str
            middle = len `div` 2
            (a, b) = splitAt middle str
blinkOnce (x:xs) = ((x * 2024):blinkOnce xs)

blink :: Int -> [Integer] -> [Integer]
blink i xs = (iterate blinkOnce xs) !! i

day_11 :: String -> (Integer, Integer)
day_11 s = (fromIntegral $ length $ blink 25 input, fromIntegral $ length $ blink 75 input)
    where input = readInput s
