module Day_02 where

parseInput :: String -> [[Integer]]
parseInput = map (map read) . map words . lines

view2 :: (a -> a -> a) -> [a] -> [a]
view2 _ [] = []
view2 f l = map (uncurry f) $ zip (init l) (drop 1 l)

atMost :: Integer -> (Integer -> Bool) -> [Integer] -> Bool
atMost i f l  = count f l <= i

count :: (Integer -> Bool) -> [Integer] -> Integer
count f xs = fromIntegral $ length $ filter f xs

getSublists :: [Integer] -> [[Integer]]
getSublists xs = map (\(x, y) -> x ++ tail y) $ 
                 map (\x -> splitAt x xs) [0..(length xs)-1]

checkValidity_a :: [Integer] -> Bool
checkValidity_a l = (all (\x -> x > 0 && x <= 3)  $ view2 (-) l) || 
                    (all (\x -> x < 0 && x >= -3) $ view2 (-) l)

checkValidity_b :: [Integer] -> Bool
checkValidity_b = any (checkValidity_a) . getSublists 

day_02_a :: [[Integer]] -> Integer
day_02_a = fromIntegral . length . filter checkValidity_a

day_02_b :: [[Integer]] -> Integer
day_02_b = fromIntegral . length . filter checkValidity_b

day_02 :: String -> (Integer, Integer)
day_02 s = (day_02_a input, day_02_b input)
    where input = parseInput s
