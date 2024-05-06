module Moves where

import Stats
import Types

data MoveSet = MoveSet { move_1 :: Maybe Move, move_2 :: Maybe Move, move_3 :: Maybe Move, move_4 :: Maybe Move}
    deriving (Eq)
instance Show MoveSet where
    show (MoveSet a b c d) = show a <> ", " <> show b <> ", " <> show c <> ", " <> show d

data Move = Move { moveName :: String, moveElem :: Types, moveType :: MoveType, movePP :: PP, movePower :: Int}
    deriving (Eq)
instance Show Move where
    show move = "Move: " <> (moveName move) <> ", "<> show (moveElem move) <> " PP: " <> show (movePP move) <> " Power: " <> show (movePower move)

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