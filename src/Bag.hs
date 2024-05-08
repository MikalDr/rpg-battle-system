module Bag where

import Homie
import Types


data Bag = Bag { potions :: Int, databrus :: Int }
    deriving (Eq, Show)

startBag = Bag { potions = 3, databrus = 0 }

hasPotions :: Bag -> Bool
hasPotions bag = (potions bag) > 0

hasDataBrus :: Bag -> Bool
hasDataBrus bag = (databrus bag) > 0

usePotion :: Homie -> Bag -> (Homie, Bag)
usePotion homie bag = 
    if hasPotions bag then
        let newHomie = healHomie 15 homie 
            newBag = removePotion bag
        in (newHomie, newBag)
    else
        (homie, bag)

useDataBrus :: Homie -> Bag -> (Homie, Bag)
useDataBrus homie bag = 
    if hasDataBrus bag then
        let newHomie = healHomie 25 homie
            newBag = removeDataBrus bag
        in (newHomie, newBag)
    else
        (homie, bag)

removePotion :: Bag -> Bag
removePotion bag = bag { potions=(potions bag-1)}

removeDataBrus :: Bag -> Bag
removeDataBrus bag = bag { databrus=(databrus bag-1)}

addPotion :: Bag -> Int -> Bag
addPotion bag amount = bag { potions=(potions bag+amount)}
