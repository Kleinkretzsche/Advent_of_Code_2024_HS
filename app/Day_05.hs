module Day_05 where

import Data.Set as S
import Prelude hiding (lookup)
import Data.Map as M
import Text.Parsec 
import Data.Functor.Identity


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
    case (M.lookup n1 m) of 
        (Just xs) -> putState $ M.update (const $ Just $ S.insert n2 xs) n1 m
        Nothing   -> putState $ M.insert n1 (S.singleton n2) m

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
    lines <- many lineOfNums
    return (s, lines)

lineInOrder :: EqMap -> [Int] -> Bool
lineInOrder _ []     = True
lineInOrder m (x:xs) = undefined

parseInput :: String -> Either ParseError (EqMap, [[Int]]) 
parseInput s = runParser (getFullInput) (M.fromList []) "" s

day_05 :: String -> (Integer, Integer)
day_05 s = undefined
