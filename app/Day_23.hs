module Day_23 where

import Data.Traversable (for)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map (Map)

type Port = (Char, Char)

type Connection  = (Port, Port)

type Connections = Map Port (Set Port)

emptyConnections :: Connections
emptyConnections = Map.fromList $ [((a,b), Set.empty) | a <- ['a'..'z'], b <- ['a'..'z']]

connectionMap :: [Connection] -> Connections
connectionMap = Map.filter (not . Set.null) . go emptyConnections
  where 
    go :: Connections -> [Connection] -> Connections
    go cs [] = cs
    go cs ((a, b):xs) = go (Map.insertWith Set.union b (Set.singleton a) $ Map.insertWith Set.union a (Set.singleton b) cs) xs

connectionFromString :: String -> Connection 
connectionFromString s = ((s !! 0, s !! 1), (s !! 3, s !! 4))

parseConnections :: String -> [Connection]
parseConnections = map connectionFromString . lines

findLoops :: Connections -> Integer
findLoops cs = 
  let 
    keysWithT = Map.toList $ Map.filterWithKey (\(c, _) _ -> c == 't') cs
    getConnections = \x -> Set.toList $ fromJust $ Map.lookup x cs 
  in 
    fromIntegral $
    sum $
    map (\(p, xs) -> 
      length $ filter (== p) $ concat $ map getConnections $ concat $ map getConnections $ Set.toList xs
    ) keysWithT 
