module Day_12 where

import qualified Util.Field as F

type Garden = F.Field Char

data Direction = U | D | L | R deriving(Show, Eq)

gardenFromString :: String -> Garden
gardenFromString = F.fromString

day_12 :: String -> (Integer, Integer)
day_12 = \_ -> (-1, -1)
