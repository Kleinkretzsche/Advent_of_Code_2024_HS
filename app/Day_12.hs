module Day_12 where

import qualified Util.Field as F

type Garden = F.Field Char

data Direction = U | D | L | R deriving(Show, Eq)

gardenFromString :: String -> Garden
gardenFromString = F.fromString

getPlotPrice :: Garden -> (Integer, Garden)
getPlotPrice g = 
  (case lookupGT (0,0) g) of 
    Just i  -> undefined
    Nothing -> undefined

day_12 :: String -> (Integer, Integer)
day_12 = \_ -> (-1, -1)
