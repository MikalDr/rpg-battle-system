module Types where

data Types = Fire | Water | Grass
    deriving (Eq, Show)

data Nature = Normal
    deriving (Eq, Show)

-- Moves

data MoveType = Physical | Special | Status
    deriving (Eq, Show)

data MoveSet = MoveSet { move_1 :: Maybe Move, move_2 :: Maybe Move, move_3 :: Maybe Move, move_4 :: Maybe Move}
    deriving (Eq)
instance Show MoveSet where
    show (MoveSet a b c d) = show a <> ", " <> show b <> ", " <> show c <> ", " <> show d

data Move = Move { moveName :: String, moveElem :: Types, moveType :: MoveType, movePP :: PP, movePower :: Int}
    deriving (Eq)
instance Show Move where
    show move = "Move: " <> (moveName move) <> ", "<> show (moveElem move) <> " PP: " <> show (movePP move) <> " Power: " <> show (movePower move)

-- Homie

data Level = Level { level :: Int, nextLevel :: Int}
    deriving (Eq)
instance Show Level where
    show = show . level

data Homie = Homie { homieName :: String, homieLevel :: Level, homieType :: Types, homieMoves :: MoveSet, homieStats :: StatBlock}
    deriving (Eq)

instance Show Homie where
    show homie = (homieName homie) <> " lvl" <> show (homieLevel homie) <> ", " <> show (hp $ homieStats homie)

-- Party

data PartyIndex = H1 | H2 | H3 | H4

data Party = Party { h1 :: Maybe Homie, h2 :: Maybe Homie, h3 :: Maybe Homie, h4 :: Maybe Homie }
instance Show Party where
    show party = show (h1 party) <> " | " <> show (h2 party) <> " | " <> show (h3 party) <> " | " <> show (h4 party)

-- Stats

data HP = HP { currentHP :: Int, maxHp :: Int }
    deriving (Eq)
instance Show HP where
    show (HP a b) = "HP: "<>show a <> "/" <> show b

data PP = PP { pp :: Int, maxPP :: Int }
    deriving (Eq)
instance Show PP where
    show (PP a b) = show a <> "/" <> show b

data Stat = ATK | DEF | SPD | SPC
    deriving (Eq)
instance Show Stat where
    show ATK = "Attack"
    show DEF = "Defense"
    show SPD = "Speed"
    show SPC = "Special"

data StatBlock = StatBlock { hp :: HP, atk :: Int, def :: Int, spd :: Int, spc :: Int, spdef :: Int}
    deriving (Eq)
instance Show StatBlock where
    show (StatBlock h a d s sc sdef) = show h <> " atk: " <> show a <> " def: " <> show d <> " spd: " <> show s <> " spc: " <> show sc <> " spdef: " <> show sdef


-- First type is attacking, second is defending type
getTypingModifier :: Types -> Types -> Float
getTypingModifier Fire Grass = 2
getTypingModifier Water Fire = 2
getTypingModifier Grass Water = 2
getTypingModifier Fire Water = 0.5
getTypingModifier Grass Fire = 0.5
getTypingModifier Water Grass = 0.5
getTypingModifier _ _ = 1