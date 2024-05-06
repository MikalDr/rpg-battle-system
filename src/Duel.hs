module Duel where

import Homie
import Moves

data Duel = Duel { playerHomie :: Homie, opponentHomie :: Homie, turnNumber :: Int}
instance Show Duel where
    show (Duel ph oh n) = show ph <> " vs. " <> show oh <> ", turn: " <> show n

testDuel :: Duel
testDuel = Duel { playerHomie=testHomie, opponentHomie=testHomie, turnNumber=0}

playerUseMove :: Int -> Duel -> (Duel, String)
playerUseMove moveSlot duel =
    let player = playerHomie duel
        opponent = opponentHomie duel
        maybeMove = getHomieMove moveSlot player
    in case maybeMove of
        Just move ->
            let (opp, damage) = targetMove player move opponent
                pho = expendMove moveSlot player
            in (Duel { playerHomie = pho, opponentHomie = opp, turnNumber = turnNumber duel + 1 }, damage)
        Nothing ->
            (Duel { playerHomie = player, opponentHomie = opponent, turnNumber = turnNumber duel }, "")


opponentUseMove :: Int -> Duel -> (Duel, String)
opponentUseMove moveSlot duel =
    let player = playerHomie duel
        opponent = opponentHomie duel
        maybeMove = getHomieMove moveSlot opponent
    in case maybeMove of
        Just move ->
            let (pho, damage) = targetMove opponent move player
                opp = expendMove moveSlot opponent
            in (Duel { playerHomie = pho, opponentHomie = opp, turnNumber = turnNumber duel + 1 }, damage)
        Nothing ->
            (Duel { playerHomie = player, opponentHomie = opponent, turnNumber = turnNumber duel }, "")
