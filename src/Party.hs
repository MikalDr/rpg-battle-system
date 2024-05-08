module Party where

import Homie
import Stats
import Data.Maybe (catMaybes)
import Types

initParty :: Party
initParty = Party { h1= Nothing, h2=Nothing, h3= Nothing, h4=Nothing }
testParty :: Party
testParty = Party { h1= Just testHomie, h2=Nothing, h3= Just testHomie2, h4=Nothing }
testParty2 :: Party
testParty2 = Party { h1= Just testHomie, h2=Nothing, h3= Nothing, h4=Nothing }

swapPartyOrder :: Int -> Int -> Party -> Party
swapPartyOrder i j party =
    let homies = partyToList party
        temp = homies !! i
        updatedList = replaceAtIndex j (homies !! i) (replaceAtIndex i (homies !! j) homies)
    in listToParty updatedList

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = take n ls ++ [item] ++ drop (n + 1) ls

listToParty :: [Homie] -> Party
listToParty hs = 
    let [h1, h2, h3, h4] = take 4 (map Just hs ++ repeat Nothing)
    in Party h1 h2 h3 h4

partyToList :: Party -> [Homie]
partyToList (Party h1 h2 h3 h4) = catMaybes [h1, h2, h3, h4]

pushHomiesForward :: Party -> Party
pushHomiesForward party =
    let homies = [h1 party, h2 party, h3 party, h4 party]
        homies' = foldr (\(x, y) acc -> if x == Nothing then y : acc else x : acc) [] $ zip homies (tail homies ++ [Nothing])
        emptyHomie = Nothing
    in Party { h1 = head homies', h2 = if h1 party == emptyHomie then Nothing else homies' !! 1, h3 = if h2 party == emptyHomie then Nothing else homies' !! 2, h4 = if h3 party == emptyHomie then Nothing else homies' !! 3 }  -- Reconstruct the party with the updated homies



getPartyMember :: Int -> Party -> Maybe Homie
getPartyMember 0 p = h1 p
getPartyMember 1 p = h2 p
getPartyMember 2 p = h3 p
getPartyMember 3 p = h4 p
getPartyMember _ p = Nothing

unsafePartyConvert :: Maybe Homie -> Homie
unsafePartyConvert (Just x) = x
unsafePartyConvert _ = testHomie

setPartyMember :: Int -> Maybe Homie -> Party -> Party
setPartyMember 0 homie party = party {h1 = homie}
setPartyMember 1 homie party = party {h2 = homie}
setPartyMember 2 homie party = party {h3 = homie}
setPartyMember 3 homie party = party {h4 = homie}
setPartyMember _ _ party = party

removePartyMember :: Int -> Party -> Party
removePartyMember ix party = setPartyMember ix Nothing party

isPartyDead :: [Homie] -> Bool
isPartyDead (x:xs) = isHPZero (homieStats x) && isPartyDead xs
isPartyDead [] = True

partySize :: Party -> Int
partySize p = length $ partyToList p

-- TODO: USE ITEM
