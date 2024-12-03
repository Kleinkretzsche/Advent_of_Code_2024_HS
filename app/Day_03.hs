module Day_03 where

import GHC.Num
import Text.Parsec 
import Data.Functor.Identity
import Data.Either

type Parser a = ParsecT String Int Identity a

num :: Parser Int
num = do 
    c <- digit
    return (read [c])

smallNum :: Parser Int
smallNum = do
    first  <- num
    second <- optionMaybe num 
    third  <- optionMaybe num 
    case second of 
        (Just x) -> case third of 
                        (Just y) -> return (first*100+x*10+y)
                        Nothing  -> return (first*10 +x)
        Nothing  -> return (first)

mulExpr :: Parser Int
mulExpr = between (string "mul(") (char ')') mul

mul :: Parser Int
mul = do
    s  <- getState
    n1 <- smallNum
    _  <- char ','
    n2 <- smallNum
    return (s*n1*n2)

skip :: Parser Int
skip = do 
    _ <- anyToken
    return 0

switchState :: Parser Int
switchState = do 
    s <- getState
    case s of 
        0 -> do _ <- string "do()"; putState 1
        _ -> do _ <- string "don't()"; putState 0
    return 0

mulExpr' :: Parser Int
mulExpr' = try (mulExpr) <|> skip

day_03_a :: Parser Int
day_03_a = do 
    res <- many mulExpr'
    return $ sum res

day_03_b :: Parser Int
day_03_b = do
    res <- many (try (switchState) <|> mulExpr')
    return $ sum res

day_03 :: String -> (Int, Int) 
day_03 s = (unwrap day_03_a s, unwrap day_03_b s)
    where 
        unwrap :: Parser Int -> String -> Int
        unwrap p str = case (runParser p 1 "" str) of
                           Right res -> res
                           Left  _   -> -1
