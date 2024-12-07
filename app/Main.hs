module Main where

import System.Environment (getArgs)
import Data.Char (isNumber)

import Day_01 (day_01)
import Day_02 (day_02)
import Day_03 (day_03)
import Day_04 (day_04)
import Day_05 (day_05)
import Day_06 (day_06)
import Day_07 

showRes :: (Integer, Integer) -> IO ()
showRes (a, b) = do
    putStrLn $ show a
    putStr $ show b

main :: IO ()
main = do
    (fname:_) <- getArgs

    input <- readFile fname

    case (readInt $ filter (isNumber) fname) of 
        1 -> showRes $ day_01 input
        2 -> showRes $ day_02 input
        3 -> showRes $ day_03 input
        4 -> showRes $ day_04 input
        5 -> showRes $ day_05 input
        6 -> showRes $ day_06 input
        7 -> showRes $ day_07 input
        x -> putStrLn $ (show x) ++ " is not a valid day"
  where 
    readInt :: String -> Integer
    readInt = read
