module Opponent where

import Duel
import Homie
import Party
import Moves

-- Different way the AI should behave when selecting Actions and Moves
data AIBehaviour = Aggresive | Status | Random | Rat | Normal
-- Aggresive -> only selects combat moves
-- Status -> Prefers to use a status move, if the opponent has no status
-- Random -> randomly selects
-- Rat -> Uses potions all the frickin time, if he can, unless Aggressive
-- Normal -> Maybe a bit decent?

-- Chooses whether the AI should Fight or use an Item, or Switch
selectAction :: Duel -> String
selectAction = undefined

-- Decides what move the AI should use.
selectOpponentMoveIndex :: [Move] -> IO Int
selectOpponentMoveIndex _ = return 1

-- Decides what Homie the AI should switch to.
selectSwitch :: Party -> Homie
selectSwitch = undefined

-- TODO: SelectItem :: Bag -> Item
