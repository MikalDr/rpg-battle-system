module Stats where

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
    show (StatBlock h a d s sc sdef) = "hp:" <> show h <> " atk: " <> show a <> " def: " <> show d <> " spd: " <> show s <> " spc: " <> show sc <> " spdef: " <> show sdef

testStatBlock :: StatBlock
testStatBlock = StatBlock { hp=(HP 5 5), atk=5, def=5, spd=5, spc=5, spdef=5}

updateHP :: Int -> HP -> HP
updateHP value (HP a b) = HP (min b (max 0 (a+value))) b

updatePP :: Int -> PP -> PP
updatePP value (PP a b) = PP (min b (max 0 (a+value))) b

updateStat :: Int -> Stat -> StatBlock -> StatBlock
updateStat amount ATK statBlock = statBlock { atk = (atk statBlock) + amount }
updateStat amount DEF statBlock = statBlock { def = (atk statBlock) + amount }
updateStat amount SPD statBlock = statBlock { spd = (atk statBlock) + amount }
updateStat amount SPC statBlock = statBlock { spc = (atk statBlock) + amount }

looseHP :: Int -> StatBlock -> StatBlock
looseHP damageTaken statBlock =  statBlock { hp = updateHP (-damageTaken) (hp statBlock) }

recoverHP :: Int -> StatBlock -> StatBlock
recoverHP amount statBlock = looseHP (-amount) statBlock

-- TODO: LEVEL UP with Random