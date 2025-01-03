module Main where

import System.Environment (getArgs)
import Data.Char (isNumber)

import Day_01 (day_01)
import Day_02 (day_02)
import Day_03 (day_03)
import Day_04 (day_04)
import Day_05 (day_05)
import Day_06 (day_06)
import Day_07 (day_07)
import Day_08 (day_08)
import Day_09 (day_09)
import Day_10 (day_10)
import Day_11 (day_11)
import Day_12 (day_12)
-- import Day_13 (day_13)
import Day_14 (day_14)
import Day_15 (day_15)
-- import Day_16 (day_16)
-- import Day_17 (day_17)
-- import Day_18 (day_18)
-- import Day_19 (day_19)
-- import Day_20 (day_20)
-- import Day_21 (day_21)
-- import Day_22 (day_22)
import Day_23
import Day_24

showRes :: (Integer, Integer) -> IO ()
showRes (a, b) = do
    putStrLn $ ("part a: " ++ show a)
    putStr $ ("part b: " ++ show b)

main :: IO ()
main = do
    (fname:_) <- getArgs

    input <- readFile fname

    case (readInt $ filter (isNumber) fname) of 
        1  -> showRes $ day_01 input
        2  -> showRes $ day_02 input
        3  -> showRes $ day_03 input
        4  -> showRes $ day_04 input
        5  -> showRes $ day_05 input
        6  -> showRes $ day_06 input
        7  -> showRes $ day_07 input
        8  -> showRes $ day_08 input
        9  -> showRes $ day_09 input
        10 -> showRes $ day_10 input
        11 -> showRes $ day_11 input
        12 -> showRes $ day_12 input
        14 -> showRes $ day_14 input
        x -> putStrLn $ (show x) ++ " is not a valid day"
  where 
    readInt :: String -> Integer
    readInt = read
