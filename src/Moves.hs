module Moves where

import Stats
import Types

standardMove :: Move
standardMove = Move { moveName = "Standard", moveElem=Fire, moveType = Physical, movePP = (PP 5 5), movePower = 10}

standardMoveSet :: MoveSet
standardMoveSet = MoveSet { move_1 = Just standardMove, move_2 = Just standardMove, move_3 = Just standardMove, move_4 = Nothing }

updateMove :: Maybe Move -> Maybe Move
updateMove (Just move) = Just (move { movePP = updatePP (-1) (movePP move) })
updateMove Nothing = Nothing

reduceMove :: Int -> MoveSet -> MoveSet
reduceMove 0 moveSet = moveSet { move_1 = updateMove (move_1 moveSet)}
reduceMove 1 moveSet = moveSet { move_2 = updateMove (move_2 moveSet)}
reduceMove 2 moveSet = moveSet { move_3 = updateMove (move_3 moveSet)}
reduceMove 3 moveSet = moveSet { move_4 = updateMove (move_4 moveSet)}
reduceMove _ moveSet = moveSet

getMoveByIndex :: MoveSet -> Int -> Maybe Move
getMoveByIndex (MoveSet m1 m2 m3 m4) idx = case idx of
    0 -> m1
    1 -> m2
    2 -> m3
    3 -> m4
    _       -> Nothing