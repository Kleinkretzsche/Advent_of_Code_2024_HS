module Main where

import System.Environment (getArgs)

import Day_01 (day_01)
import Day_02

main :: IO ()
main = do
    args <- getArgs
    case args of
        "01" : _ -> day_01
        "02" : _ -> day_02
        []       -> putStrLn "enter the number of the day, you want to test.."
        _        -> putStrLn "not implemented yet"
