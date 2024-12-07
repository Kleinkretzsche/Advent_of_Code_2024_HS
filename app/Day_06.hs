module Day_06 where

import Prelude hiding (lookup)
import Data.Map as M
import Data.Set as S

data Field2 t = F2 (M.Map (Int,Int) t) (Int, Int) deriving(Eq)

instance Show t => Show (Field2 t) where
    show (F2 m dim) = 
        case (sequence (sequence <$> [[(lookup (y, x) m) | x <- [1..snd dim]]| y <- [1..fst dim]])) of 
            Just xs -> "\n" ++ (unlines $ fmap unwords $ fmap (show <$>) xs)
            Nothing -> ""

emptyField :: Field2 a -> Bool
emptyField (F2 m _) = M.null m

find :: Field2 a -> (Int, Int) -> Maybe a
find (F2 m _) i = M.lookup i m

getDimField :: Field2 a -> (Int, Int)
getDimField (F2 _ dim) = dim

indexWhere :: (a -> Bool) -> Field2 a -> [(Int, Int)]
indexWhere f (F2 m dim) = Prelude.filter pr [(a,b) | a <- [0..snd dim], b <- [0..snd dim]]
    where 
        pr x = 
            case (x `M.lookup` m) of 
                Just el -> f el
                Nothing -> False

fieldFromList :: [[a]] -> Field2 a
fieldFromList xs = F2 (M.fromList $ zip [(a,b) | a <- [1..y], b <-[1..x]] (concat xs)) (y, x)
    where y  = length  xs
          x  = length (xs !! 0)

data Direction = U | D | L | R deriving(Show, Eq)

data Guard = Guard 
    { patrol :: Field2 Char
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
look (Guard m p _ l) = find m (addTup p $ dirToOffs l)

lookLeft :: Guard -> Maybe Char
lookLeft (Guard m p _ l) = find m (addTup p $ dirToOffs (iterate turn90 l !! 3))

step :: Guard -> Guard
step g@(Guard f p s l) = 
    case (look g) of 
        Just '#' -> Guard f (addTup p $ dirToOffs (turn90 l)) (p `S.insert` s) (turn90 l)
        Just _   -> Guard f (addTup p $ dirToOffs l) (p `S.insert` s) l 
        Nothing  -> Guard (F2 (M.fromList []) (0,0)) (-1,-1) (p `S.insert` s) l 

stepW :: Guard -> Guard
stepW gu = until (\x -> (lookLeft x) == (Just '#') || (pos x) == (-1, -1)) step gu

obstructionOrder :: Guard -> [(Int, Int)]
obstructionOrder (Guard _ (-1, -1) _ _) = []
obstructionOrder g = if (facing g) /= (facing g') then bl:(obstructionOrder g') else obstructionOrder g'
    where 
        g' = step g
        bl = addTup (pos g) (dirToOffs $ facing g)

detectLoop :: Guard -> Bool
detectLoop g = (-1, -1) == (pos $ snd $ until (\(vis, p) -> (pos p) == (-1, -1) || (pos p) `elem` vis) 
                                              (\(vis, p) -> ((pos p):vis, stepW p)) ([],g))

potentialBlocks :: [(Int, Int)] -> [(Int, Int)]
potentialBlocks ls = (\((_, x1), (_, x2), (y3, x3)) -> ((y3-1, x3 + (x1 - x2)))) <$> (view3 ls)

view3 :: [a] -> [(a, a, a)]
view3 xs = zip3 xs (Prelude.drop 1 xs) (Prelude.drop 2 xs)

mkGuard :: String -> Guard
mkGuard s = Guard f ((indexWhere (== '^') f) !! 0) S.empty U
    where f = fieldFromList $ concat $ words <$> lines s

day_06_a :: Guard -> Int
day_06_a g = length $ S.toList $ seen $ until ((== (-1, -1)) . pos) step g

validWithBlock :: Guard -> (Int, Int) -> Bool
validWithBlock (Guard (F2 m dim) p s f) coord = detectLoop (Guard (F2 (M.insert coord '#' m) dim) p s f)

brute :: Guard -> [(Int, Int)]
brute g = S.toList $ allPos `S.difference` obstructed
    where
        (y, x)     = (getDimField . patrol) g
        allPos     = S.fromList $ [(y', x') | y' <- [1..y], x' <- [1..x]]
        obstructed = S.fromList $ (pos g):(indexWhere (== '#') (patrol g))

day_06_b :: Guard -> Int
-- day_06_b g = length $ Prelude.filter (not . validWithBlock g) (potentialBlocks $ obstructionOrder g)
day_06_b g = length $ Prelude.filter (not . validWithBlock g) (brute g)
        
day_06 :: String -> (Integer, Integer)
day_06 s = (fromIntegral $ day_06_a g, fromIntegral $ day_06_b g)
    where g = mkGuard s
