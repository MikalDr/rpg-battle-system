module Homie where

import Types
import Stats
import Moves

data Level = Level { level :: Int, nextLevel :: Int}
    deriving (Eq)
instance Show Level where
    show = show . level

data Homie = Homie { homieName :: String, homieLevel :: Level, homieType :: Types, homieMoves :: MoveSet, homieStats :: StatBlock}
    deriving (Eq)

instance Show Homie where
    show homie = (homieName homie) <> " lvl" <> show (homieLevel homie) <> ", " <> show (hp $ homieStats homie)

testHomie :: Homie
testHomie = Homie { homieName = "Kaspar", homieLevel=(Level 5 25), homieType = Fire, homieMoves = standardMoveSet, homieStats = testStatBlock}
testHomie2 :: Homie
testHomie2 = Homie { homieName = "Nils", homieLevel=(Level 5 25), homieType = Water, homieMoves = standardMoveSet, homieStats = testStatBlock}

getHomieAtk :: Homie -> Int
getHomieAtk = atk . homieStats

getHomieDef :: Homie -> Int
getHomieDef = def . homieStats

getHomieSpc :: Homie -> Int
getHomieSpc = spc . homieStats

getHomieSpcDef :: Homie -> Int
getHomieSpcDef = spdef . homieStats

getHomieLevel :: Homie -> Int
getHomieLevel = level . homieLevel

getHomieSpd :: Homie -> Int
getHomieSpd = spd . homieStats

getHomieMoveSet :: Homie -> MoveSet
getHomieMoveSet = homieMoves

getHomieMove :: Int -> Homie -> Maybe Move
getHomieMove 0 = move_1 . homieMoves
getHomieMove 1 = move_2 . homieMoves
getHomieMove 2 = move_3 . homieMoves
getHomieMove 3 = move_4 . homieMoves

hurtHomie :: Int -> Homie -> Homie
hurtHomie amount homie = homie { homieStats = looseHP amount (homieStats homie)}

healHomie :: Int -> Homie -> Homie
healHomie amount homie = homie { homieStats = recoverHP amount (homieStats homie)}

-- Uses the move PP on a homie
expendMove :: Int -> Homie -> Homie
expendMove idx homie = homie { homieMoves = reduceMove idx (homieMoves homie)}

-- The pokemon damage fomula
-- Damage = (((((2 * Level / 5) + 2) * Power * Attack / Defense) / 50) + 2) * Modifier
-- TODO: ADD SOME RANDOMNESS TO DAMAGE
damageFormula :: Int -> Move -> Int -> Int -> Float -> Int 
damageFormula oppStat move homStat level mod = round ((((2 * level' / 5) + 2) * power * oppStat' / homStat' / 50 + 2) * mod')
    where level' = fromIntegral level
          oppStat' = fromIntegral oppStat
          homStat' = fromIntegral homStat
          mod' = mod
          power = fromIntegral (movePower move)

-- Causes a move effect on a Homie (Opponent)
-- TODO: ADD SOME RANDOMNESS TO ACCURACY OR HIT RATIO
targetMove :: Homie -> Move -> Homie -> (Homie, String)
-- Physical Move
targetMove atkHomie move defHomie | moveType move == Physical = (hurtHomie damage defHomie, (show damage))
                                  | moveType move == Special = (hurtHomie damage defHomie, (show damage))
                                  where typingModifier = getTypingModifier (homieType atkHomie) (homieType defHomie)
                                        damage = damageFormula (getHomieSpc atkHomie) move (getHomieSpcDef defHomie) (getHomieLevel atkHomie) typingModifier
-- Status Move