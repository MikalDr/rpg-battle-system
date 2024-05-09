{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Homie
import Types
import Stats
import Moves
import Bag
import System.Random (randomRIO)

instance Arbitrary Homie where
  arbitrary = do
    name <- elements ["Kaspar", "Nils", "Einem"]
    let level = Level 5 25
    typ <- elements [Fire, Water, Grass]  -- Use '<-' to extract from Gen Types
    let moveSet = standardMoveSet  -- Ensure this is appropriately defined
    let statBlock = testStatBlock  -- Ensure this is appropriately defined
    return $ Homie name level typ moveSet statBlock

-- Types: Fire, Water, Grass
instance Arbitrary Types where
    arbitrary = elements [Fire, Water, Grass]

-- MoveType: Physical, Special, Status
instance Arbitrary MoveType where
    arbitrary = elements [Physical, Special, Status]

-- PP
instance Arbitrary PP where
    arbitrary = PP <$> choose (1, 100) <*> choose (1, 100)

-- Move
instance Arbitrary Move where
    arbitrary = Move <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> choose (1, 100)

-- Level
instance Arbitrary Level where
    arbitrary = Level <$> choose (1, 100) <*> choose (1, 200)

-- StatBlock
instance Arbitrary StatBlock where
    arbitrary = StatBlock <$> arbitrary <*> choose (1, 100) <*> choose (1, 100) <*> choose (1, 100) <*> choose (1, 100) <*> choose (1, 100)

-- HP
instance Arbitrary HP where
    arbitrary = HP <$> choose (1, 100) <*> choose (1, 100)

-- MoveSet
instance Arbitrary MoveSet where
    arbitrary = MoveSet <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Ensure healing a Homie never overflows beyond the max HP
prop_healHomieNeverOverflows :: Homie -> NonNegative Int -> Property
prop_healHomieNeverOverflows homie (NonNegative amount) = monadicIO $ do
    let healedHomie = healHomie amount homie
    let currentHPVal = currentHP $ hp $ homieStats healedHomie
    let maxHPVal = maxHp $ hp $ homieStats homie
    assert (currentHPVal <= maxHPVal)

-- Ensure damaging a Homie never drops HP below zero
prop_hurtHomieNeverNegative :: Homie -> Positive Int -> Property
prop_hurtHomieNeverNegative homie (Positive amount) = monadicIO $ do
    let injuredHomie = hurtHomie amount homie
    let currentHPVal = currentHP $ hp $ homieStats injuredHomie
    assert (currentHPVal >= 0)

-- Verify that level up increases all stats
prop_levelUpIncreasesStats :: Homie -> Property
prop_levelUpIncreasesStats homie = monadicIO $ do
    leveledUpHomie <- run $ levelUp homie
    let oldStats = homieStats homie
    let newStats = homieStats leveledUpHomie
    assert $ and [atk newStats > atk oldStats, def newStats > def oldStats, spd newStats > spd oldStats, spc newStats > spc oldStats, spdef newStats > spdef oldStats]

-- Check damage calculation is correct based on type effectiveness
prop_damageEffectiveness :: Homie -> Move -> Homie -> Property
prop_damageEffectiveness atkHomie move defHomie = monadicIO $ do
    (_, damageStr) <- run $ targetMove atkHomie move defHomie
    let expectedMod = getTypingModifier (homieType atkHomie) (homieType defHomie)
    let baseDamage = round ((fromIntegral (movePower move) * expectedMod) :: Float) :: Int
    let damage = read damageStr :: Int
    assert $ damage >= round ((fromIntegral baseDamage * 0.85) :: Float) && damage <= baseDamage

-- Testing getHomieAtk returns the attack stat from Homie's StatBlock
prop_getHomieAtk :: Homie -> Property
prop_getHomieAtk homie = getHomieAtk homie === atk (homieStats homie)

-- Testing getHomieDef returns the defense stat from Homie's StatBlock
prop_getHomieDef :: Homie -> Property
prop_getHomieDef homie = getHomieDef homie === def (homieStats homie)

-- Testing getHomieSpc returns the special stat from Homie's StatBlock
prop_getHomieSpc :: Homie -> Property
prop_getHomieSpc homie = getHomieSpc homie === spc (homieStats homie)

-- Testing getHomieSpcDef returns the special defense stat from Homie's StatBlock
prop_getHomieSpcDef :: Homie -> Property
prop_getHomieSpcDef homie = getHomieSpcDef homie === spdef (homieStats homie)

-- Testing getHomieLevel returns the level from Homie's Level
prop_getHomieLevel :: Homie -> Property
prop_getHomieLevel homie = getHomieLevel homie === level (homieLevel homie)

-- Testing getHomieSpd returns the speed stat from Homie's StatBlock
prop_getHomieSpd :: Homie -> Property
prop_getHomieSpd homie = getHomieSpd homie === spd (homieStats homie)

-- Testing getHomieMoveSet returns the MoveSet from Homie
prop_getHomieMoveSet :: Homie -> Property
prop_getHomieMoveSet homie = getHomieMoveSet homie === homieMoves homie

-- Testing getHomieMove returns the correct move based on index
prop_getHomieMove :: Homie -> NonNegative Int -> Property
prop_getHomieMove homie (NonNegative idx) = monadicIO $ do
    let move = getHomieMove (idx `mod` 4) homie  -- Modulo 4 since there are 4 possible moves
    let expectedMove = case idx `mod` 4 of
            0 -> move_1 (homieMoves homie)
            1 -> move_2 (homieMoves homie)
            2 -> move_3 (homieMoves homie)
            3 -> move_4 (homieMoves homie)
    assert (move == expectedMove)

return []
runTests = $quickCheckAll

main :: IO ()
main = do
    putStrLn "Running tests..."
    runTests >>= print

