module Day_06 where

import qualified Util.Field as F
import qualified Data.Map as M
import qualified Data.Set as S

data Direction = U | D | L | R deriving(Show, Eq)

data Guard = Guard 
    { patrol :: F.Field Char
    , pos :: (Int, Int)
    , seen :: S.Set (Int, Int)
    , facing :: Direction
    } deriving(Show)

dirToOffs :: Direction -> (Int, Int)
dirToOffs U = (-1,  0)
dirToOffs R = ( 0,  1)
dirToOffs D = ( 1,  0)
dirToOffs L = ( 0, -1)

turn90 :: Direction -> Direction
turn90 U = R
turn90 R = D
turn90 D = L
turn90 L = U

addTup :: Num a => (a, a) -> (a, a) -> (a, a)
addTup (x, y) (u, v) = (x+u, y+v)

look :: Guard -> Maybe Char
look (Guard m p _ l) = F.find m (addTup p $ dirToOffs l)

lookLeft :: Guard -> Maybe Char
lookLeft (Guard m p _ l) = F.find m (addTup p $ dirToOffs (iterate turn90 l !! 3))

turn :: Guard -> Guard 
turn (Guard f p s l) = Guard f p s (turn90 l)

step :: Guard -> Guard
step g@(Guard f p s l) = 
    case (look g) of 
        Just '#' -> turn g
        Just _   -> Guard f (addTup p $ dirToOffs l) (p `S.insert` s) l 
        Nothing  -> Guard (F.MkField (M.fromList []) (0,0)) (-1,-1) (p `S.insert` s) l 

stepW :: Guard -> Guard
stepW gu = 
    case (look gu') of
        Just '#' -> until (\x -> (look x) /= (Just '#')) turn gu'
        _        -> gu'
    where gu' = until (\x -> (look x) == (Just '#') || (pos x) == (-1, -1)) step gu

mkGuard :: String -> Guard
mkGuard s = Guard f ((F.indexWhere (== '^') f) !! 0) S.empty U
    where f = F.fromString s

day_06_a :: Guard -> Int
day_06_a g = length $ S.toList $ seen $ until ((== (-1, -1)) . pos) stepW g

detectLoop :: Guard -> Bool
detectLoop g = (-1, -1) /= (pos $ snd $ until (\(vis, p) -> (pos p) == (-1, -1) || (pos p) `elem` vis) 
                                              (\(vis, p) -> ((pos p):vis, stepW p)) ([],g))

guardGetsCaught :: Guard -> (Int, Int) -> Bool
guardGetsCaught (Guard (F.MkField m dim) p s l) i = detectLoop (Guard (F.MkField (M.insert i '#' m) dim) p s l)

brute :: Guard -> [(Int, Int)]
brute g = S.toList $ allPos `S.difference` obstructed
    where
        (y, x)     = (F.dim. patrol) g
        allPos     = S.fromList $ [(y', x') | y' <- [1..y], x' <- [1..x]]
        obstructed = S.fromList $ (pos g):(F.indexWhere (== '#') (patrol g))

day_06_bt :: Guard -> [(Int, Int)]
day_06_bt g = filter (guardGetsCaught g) (brute g)

day_06_b :: Guard -> Int
day_06_b g = length $ Prelude.filter (guardGetsCaught g) (brute g)        

day_06 :: String -> (Integer, Integer)
day_06 s = (fromIntegral $ day_06_a g, (fromIntegral $ day_06_b g) - 1)
    where g = mkGuard s
