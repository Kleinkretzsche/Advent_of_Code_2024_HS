module Day_14 where

import Text.Parsec
import Data.Either (fromRight)

import qualified Util.Field as F

type Vec2 = (Int, Int)
type Vec4 = (Int, Int, Int, Int)

type Parser = Parsec String ()

data Robot = MkRobot 
  { pos :: Vec2
  , vel :: Vec2
  } deriving (Show, Eq)

vec2 :: Parser Vec2
vec2 = do
  n  <- anyChar
  ns <- many digit
  _  <- char ','
  m  <- anyChar
  ms <- many digit
  return (read (n:ns), read (m:ms))

robot :: Parser Robot
robot = do
  _ <- string "p="
  p <- vec2
  _ <- string " v="
  v <- vec2
  _ <- char '\n'
  return $ MkRobot p v

calcPositions :: Int -> Vec2 -> [Robot] -> [Robot]
calcPositions i (y, x) = fmap update 
  where 
    update (MkRobot p v) = MkRobot 
                           (diMapTup ((`mod` y), (`mod` x)) $
                           p .+. 
                           mapTup (*i) v)
                           v

diMapTup :: ((Int -> Int),(Int -> Int)) -> (Int, Int) -> (Int, Int)
diMapTup (f, g) (a, b) = (f a, g b)
    
mapTup :: (Int -> Int) -> (Int, Int) -> (Int, Int)
mapTup f = diMapTup (f, f)

(.+.) :: Vec2 -> Vec2 -> Vec2
(a1, a2) .+. (b1, b2) = (a1 + b1, a2 + b2)

(..+..) :: Vec4 -> Vec4 -> Vec4
(a1, a2, a3, a4) ..+.. (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)

countQuadrants :: (Int, Int) -> [Robot] -> Vec4
countQuadrants (ym, xm) = foldl1 (..+..) . map quadrant
  where 
    yh = ym `div` 2
    xh = xm `div` 2
    quadrant (MkRobot (y, x) _)
      | y < yh && x < xh = (1, 0, 0, 0) 
      | y < yh && x > xh = (0, 1, 0, 0) 
      | y > yh && x < xh = (0, 0, 1, 0) 
      | y > yh && x > xh = (0, 0, 0, 1) 
      | otherwise        = (0, 0, 0, 0) 

day_14_a :: [Robot] -> Integer
day_14_a = 
  let dim = (103, 101)
  in fromIntegral . (\(a,b,c,d) -> a*b*c*d) . countQuadrants dim . calcPositions 100 dim

readInput :: String -> [Robot]
readInput = fromRight [] . runParser (many robot) () ""
