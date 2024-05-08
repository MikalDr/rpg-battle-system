module Opponent where

import Duel
import Homie
import Party
import Types
import Moves

-- Different way the AI should behave when selecting Actions and Moves
data AIBehaviour = AIAggresive | AIStatus | AIRandom | AIRat | AINormal
    deriving (Show, Eq)
-- Aggresive -> only selects combat moves
-- Status -> Prefers to use a status move, if the opponent has no status
-- Random -> randomly selects
-- Rat -> Uses potions all the frickin time, if he can, unless Aggressive
-- Normal -> Maybe a bit decent?

data OpponentAction = OpponentAttack | OpponentSwitch | OpponentBag
    deriving (Show, Eq)

-- Chooses whether the AI should Fight or use an Item, or Switch
selectOpponentAction :: Duel -> Party -> OpponentAction
selectOpponentAction _ _ = OpponentAttack

-- Decides what move the AI should use.
selectOpponentMoveIndex :: [Move] -> IO Int
selectOpponentMoveIndex _ = return 1

-- Decides what Homie the AI should switch to.
selectSwitch :: Party -> Homie
selectSwitch _ = testHomie

-- TODO: SelectItem :: Bag -> Item
