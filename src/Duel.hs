module Duel where

import Homie
import Moves
import Types
import Party

data Duel = Duel { playerHomie :: Homie, opponentHomie :: Homie, turnNumber :: Int}
instance Show Duel where
    show (Duel ph oh n) = show ph <> " vs. " <> show oh <> ", turn: " <> show n

testDuel :: Duel
testDuel = Duel { playerHomie=testHomie, opponentHomie=testHomie, turnNumber=0}

initDuel :: Party -> Party -> Duel
initDuel pp _ = Duel { playerHomie=(unsafePartyConvert $ getPartyMember 0 pp), opponentHomie=testHomie, turnNumber=0}

playerUseMove :: Int -> Duel -> IO (Duel, String)
playerUseMove moveSlot duel = do
    let player = playerHomie duel
        opponent = opponentHomie duel
        maybeMove = getHomieMove moveSlot player
    case maybeMove of
        Just move -> do
            (opp, damage) <- targetMove player move opponent
            let pho = expendMove moveSlot player
            return (Duel { playerHomie = pho, opponentHomie = opp, turnNumber = turnNumber duel + 1 }, damage)
        Nothing ->
            return (Duel { playerHomie = player, opponentHomie = opponent, turnNumber = turnNumber duel }, "")



opponentUseMove :: Int -> Duel -> IO (Duel, String)
opponentUseMove moveSlot duel = do
    let player = playerHomie duel
        opponent = opponentHomie duel
        maybeMove = getHomieMove moveSlot opponent
    case maybeMove of
        Just move -> do
            (pho, damage) <- targetMove opponent move player
            let opp = expendMove moveSlot opponent
            return (Duel { playerHomie = pho, opponentHomie = opp, turnNumber = turnNumber duel + 1 }, damage)
        Nothing ->
            return (Duel { playerHomie = player, opponentHomie = opponent, turnNumber = turnNumber duel }, "")