module Day_24 where

data Cable = ON | OFF | NONE deriving(Show)

data Gate = XOR (Cable -> Cable -> Cable) |
            OR  (Cable -> Cable -> Cable) |
            AND (Cable -> Cable -> Cable)

mkOR :: Gate 
mkOR = OR (or')
  where 
    or' ON  ON  = ON
    or' OFF ON  = ON 
    or' ON  OFF = ON 
    or' OFF OFF = OFF 
    or' _   _   = NONE

mkAND :: Gate 
mkAND = AND (aNd')
  where 
    aNd' ON  ON  = ON
    aNd' OFF ON  = OFF 
    aNd' ON  OFF = OFF 
    aNd' OFF OFF = ON
    aNd' _   _   = NONE

mkXOR :: Gate 
mkXOR = XOR (xor')
  where 
    xor' ON  ON  = OFF
    xor' OFF ON  = ON 
    xor' ON  OFF = ON 
    xor' OFF OFF = OFF 
    xor' _   _   = NONE

data EvalTree = Node {label :: String, gate :: Gate, left :: EvalTree, right :: EvalTree} | 
                Input {label :: String, input :: Cable}

mkNode :: String -> EvalTree
mkNode s = undefined


day_24 :: String -> (Integer, Integer)
day_24 = \_ -> (-1, -1)
