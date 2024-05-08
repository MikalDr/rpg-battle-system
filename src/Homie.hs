module Homie where

import Types
import Stats
import Moves
import System.Random (randomRIO)

testHomie :: Homie
testHomie = Homie { homieName = "Kaspar", homieLevel=(Level 5 25), homieType = Fire, homieMoves = standardMoveSet, homieStats = testStatBlock}
testHomie2 :: Homie
testHomie2 = Homie { homieName = "Nils", homieLevel=(Level 5 25), homieType = Water, homieMoves = standardMoveSet, homieStats = testStatBlock}
testHomie3 :: Homie
testHomie3 = Homie { homieName = "Einem", homieLevel=(Level 5 25), homieType = Grass, homieMoves = standardMoveSet, homieStats = testStatBlock}

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

zeroHPHomie :: Homie -> Bool
zeroHPHomie homie = (currentHP$ hp $homieStats $ homie) <= 0

hurtHomie :: Int -> Homie -> Homie
hurtHomie amount homie = homie { homieStats = looseHP amount (homieStats homie)}

healHomie :: Int -> Homie -> Homie
healHomie amount homie = homie { homieStats = recoverHP amount (homieStats homie)}

-- Uses the move PP on a homie
expendMove :: Int -> Homie -> Homie
expendMove idx homie = homie { homieMoves = reduceMove idx (homieMoves homie)}

-- The pokemon damage fomula
-- Damage = (((((2 * Level / 5) + 2) * Power * Attack / Defense) / 50) + 2) * Modifier
damageFormula :: Int -> Move -> Int -> Int -> Float -> IO Int
damageFormula oppStat move homStat level mod = do
    let level' = fromIntegral level :: Float
        oppStat' = fromIntegral oppStat :: Float
        homStat' = fromIntegral homStat :: Float
        mod' = mod :: Float
        power = fromIntegral (movePower move) :: Float
        baseDamage = ((2 * level' / 5 + 2) * power * oppStat' / homStat' / 50 + 2) * mod' :: Float

    randomFactor <- randomRIO (0.85, 1.0) :: IO Float
    return $ round (baseDamage * randomFactor)

-- Causes a move effect on a Homie (Opponent)
targetMove :: Homie -> Move -> Homie -> IO (Homie, String)
targetMove atkHomie move defHomie = do
    let typingModifier = getTypingModifier (homieType atkHomie) (homieType defHomie)
    damage <- damageFormula (getHomieSpc atkHomie) move (getHomieSpcDef defHomie) (getHomieLevel atkHomie) typingModifier
    let hurtHomieResult = hurtHomie damage defHomie
    return (hurtHomieResult, show damage)

randomInt :: IO Int
randomInt = randomRIO(1, 4)

levelUp :: Homie -> IO Homie
levelUp homie = do
    rndInt <- randomInt
    rndInt2 <- randomInt
    rndInt3 <- randomInt
    rndInt4 <- randomInt
    rndInt5 <- randomInt
    rndInt6 <- randomInt
    let stats = homieStats homie
        clevel = homieLevel homie
        newHp = (hp stats) { maxHp = maxHp (hp stats) + rndInt, currentHP = currentHP (hp stats) }
        newStats = stats {
            hp = newHp,
            atk = atk stats + rndInt2,
            def = def stats + rndInt3,
            spd = spd stats + rndInt4,
            spc = spc stats + rndInt5,
            spdef = spdef stats + rndInt6
        }
        newLevel = clevel{level=(level clevel)+1}
    return homie { homieLevel = newLevel, homieStats = newStats }

homieDeadString :: Homie -> String
homieDeadString homie | (currentHP $ hp $ homieStats homie) <= 0 = " [DEAD]"
                      | otherwise = ""

-- Status Move