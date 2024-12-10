module Day_08 where

import Data.List (nub)
import qualified Data.Set as S
import qualified Util.Field as F

antinodes :: F.Field Char -> Char -> S.Set (Int, Int)
antinodes f c = 
    S.filter (`F.validIndex` f) $
    S.fromList $ concat $ 
    [[(y1 - dy, x1 - dx), (y2 + dy, x2 + dx)] | 
        a@(y1, x1) <- ixs, 
        b@(y2, x2) <- ixs, 
        let (dy, dx) = (y2 - y1, x2 - x1),
        (a /= b)]
    where ixs = F.indexWhere (== c) f

longAntinodes :: F.Field Char -> Char -> S.Set (Int, Int)
longAntinodes f c = 
    S.filter (`F.validIndex` f) $
    S.fromList $ concat $ 
    [[(y1 - (d * dy), x1 - (d * dx)), (y2 + (d * dy), x2 + (d * dx))] | 
        a@(y1, x1) <- ixs, 
        b@(y2, x2) <- ixs, 
        let (dy, dx) = (y2 - y1, x2 - x1),
        d <- [0..lim], 
        (a /= b)]
    where ixs = F.indexWhere (== c) f
          lim = max (fst $ F.dim f) (snd $ F.dim f)

day_08_a :: String -> Integer
day_08_a s = fromIntegral $ S.size $ S.unions $ (antinodes field) <$> chars
    where 
        field = F.fromString s
        chars = nub $ filter (/= '.') s

day_08_b :: String -> Integer
day_08_b s = fromIntegral $ S.size $ S.unions $ (longAntinodes field) <$> chars
    where 
        field = F.fromString s
        chars = nub $ filter (/= '.') s

day_08 :: String -> (Integer, Integer)
day_08 s= (day_08_a s, day_08_b s)
