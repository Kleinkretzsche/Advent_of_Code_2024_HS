module Day_05 where

import Data.Either (fromRight)
import Data.Set as S
import Prelude hiding (lookup)
import Data.Map as M
import Text.Parsec 


type Parser = Parsec String EqMap
type EqMap  = M.Map Int (S.Set Int)

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

parseEq :: Parser ()
parseEq = do
    n1 <- num
    _  <- char '|'
    n2 <- num
    _  <- char '\n'
    m  <- getState
    case (M.lookup n2 m) of 
        (Just xs) -> putState $ M.update (const $ Just $ S.insert n1 xs) n2 m
        Nothing   -> putState $ M.insert n2 (S.singleton n1) m

lineOfNums :: Parser [Int]
lineOfNums = do
    xs <- sepEndBy num (char ',')
    _  <- try (char '\n')
    return xs

fillParser :: Parser ()
fillParser = do
    _ <- manyTill parseEq (try $ char '\n')
    return ()

getFullInput :: Parser (EqMap, [[Int]])
getFullInput = do
    fillParser
    s <- getState
    ls <- many lineOfNums
    return (s, ls)

lineInOrder :: EqMap -> [Int] -> Bool
lineInOrder _ [] = True
lineInOrder m xs = go m xs S.empty 
    where 
        go :: EqMap -> [Int] -> S.Set Int -> Bool
        go _ []     _   = True
        go m' (x:xs') bad = if not (x `elem` bad) 
                          then case (M.lookup x m) of
                                   (Just l) -> True && go m' xs' (bad `S.union` l)
                                   Nothing  -> True && go m' xs' bad
                          else False

takeMiddle :: [a] -> a
takeMiddle xs = xs !! ((length xs) `div` 2)

sortBy :: EqMap -> [Int] -> [Int]
sortBy m xs = go m [] xs S.empty 
    where 
        go :: EqMap -> [Int] -> [Int]-> S.Set Int -> [Int]
        go _ r [] _       = r
        go m' p (x:xs') bad = 
            if not (x `elem` bad) 
            then case (M.lookup x m) of
                (Just l) -> go m' (p<>[x]) xs' (bad `S.union` l)
                Nothing  -> go m' (p<>[x]) xs' bad
            else go m [] ((init p) ++ [x] ++ [last p] ++ xs') S.empty

parseInput :: String -> Either ParseError (EqMap, [[Int]]) 
parseInput s = runParser (getFullInput) (M.fromList []) "" s

day_05_a :: EqMap -> [[Int]] -> Int
day_05_a m ls = sum $ takeMiddle <$> Prelude.filter (lineInOrder m) ls

day_05_b :: EqMap -> [[Int]] -> Int
day_05_b m ls = sum $ (takeMiddle . sortBy m) <$> Prelude.filter (not . lineInOrder m) ls

day_05 :: String -> (Integer, Integer)
day_05 s = (fromIntegral $ day_05_a m ls, fromIntegral $ day_05_b m ls)
    where 
        (m, ls) = fromRight (M.fromList [], [])$ parseInput s
