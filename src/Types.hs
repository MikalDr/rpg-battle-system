module Types where

data Types = Fire | Water | Grass
    deriving (Eq, Show)

data Nature = Normal
    deriving (Eq, Show)

data MoveType = Physical | Special | Status
    deriving (Eq, Show)


-- First type is attacking, second is defending type
getTypingModifier :: Types -> Types -> Float
getTypingModifier Fire Grass = 2
getTypingModifier Water Fire = 2
getTypingModifier Grass Water = 2
getTypingModifier Fire Water = 0.5
getTypingModifier Grass Fire = 0.5
getTypingModifier Water Grass = 0.5
getTypingModifier _ _ = 1