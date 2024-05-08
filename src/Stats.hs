module Stats where

import Types

testStatBlock :: StatBlock
testStatBlock = StatBlock { hp=(HP 5 30), atk=5, def=5, spd=5, spc=5, spdef=5}

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

isHPZero :: StatBlock -> Bool
isHPZero statBlock = currentHP (hp statBlock) == 0

-- TODO: LEVEL UP with Random