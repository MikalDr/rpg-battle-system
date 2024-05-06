module Party where

import Homie

data PartyIndex = H1 | H2 | H3 | H4
data Party = Party { h1 :: Maybe Homie, h2 :: Maybe Homie, h3 :: Maybe Homie, h4 :: Maybe Homie }
instance Show Party where
    show party = show (h1 party) <> " | " <> show (h2 party) <> " | " <> show (h3 party) <> " | " <> show (h4 party)

testParty :: Party
testParty = Party { h1= Just testHomie, h2=Nothing, h3= Just testHomie2, h4=Nothing }

swapPartyOrder :: PartyIndex -> PartyIndex -> Party -> Party
swapPartyOrder pi pi' party = 
    let maybeHomie = getPartyMember pi party
    in case maybeHomie of
        Just homie ->
            let maybeHomie' = getPartyMember pi' party
            in case maybeHomie' of
                Just homie' ->
                    let party' = pushHomiesForward $ setPartyMember pi maybeHomie' $ setPartyMember pi' maybeHomie party
                    in party'
                Nothing -> pushHomiesForward $ party
        Nothing ->
            pushHomiesForward $ party

pushHomiesForward :: Party -> Party
pushHomiesForward party =
    let homies = [h1 party, h2 party, h3 party, h4 party]
        homies' = foldr (\(x, y) acc -> if x == Nothing then y : acc else x : acc) [] $ zip homies (tail homies ++ [Nothing])
        emptyHomie = Nothing
    in Party { h1 = head homies', h2 = if h1 party == emptyHomie then Nothing else homies' !! 1, h3 = if h2 party == emptyHomie then Nothing else homies' !! 2, h4 = if h3 party == emptyHomie then Nothing else homies' !! 3 }  -- Reconstruct the party with the updated homies



getPartyMember :: PartyIndex -> Party -> Maybe Homie
getPartyMember H1 p = h1 p
getPartyMember H2 p = h2 p
getPartyMember H3 p = h3 p
getPartyMember H4 p = h4 p

setPartyMember :: PartyIndex -> Maybe Homie -> Party -> Party
setPartyMember H1 homie party = party {h1 = homie}
setPartyMember H2 homie party = party {h2 = homie}
setPartyMember H3 homie party = party {h3 = homie}
setPartyMember H4 homie party = party {h4 = homie}

removePartyMember :: PartyIndex -> Party -> Party
removePartyMember ix party = setPartyMember ix Nothing party

-- TODO: USE ITEM
