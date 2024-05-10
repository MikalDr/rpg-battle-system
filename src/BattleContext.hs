module BattleContext where

import Duel
import Party
import Opponent
import Homie
import Moves
import Utils
import Stats
import Types
import Bag
import Control.Monad.State
import Data.Char (toLower)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

data BattleContext = BattleContext { duel :: Duel, playerParty :: Party, playerBag :: Bag, opponentParty :: Party, opponentType :: AIBehaviour, gameQuit :: Bool, winner :: Bool}

data TurnOrder = Player | Equal | Opponent
    deriving (Show, Eq)

type BattleState = StateT BattleContext IO

testContext = BattleContext {duel = testDuel, playerParty = testParty, playerBag=startBag, opponentParty = testParty2, opponentType = AINormal, gameQuit = False, winner = False}

initContext :: Party -> Party -> Bag -> BattleContext
-- Alot of variables
initContext pp op bag = BattleContext {duel = (initDuel pp op) , playerParty = pp, playerBag=bag, opponentParty = testParty2, opponentType = AINormal, gameQuit = False, winner = False}

initBattleState :: BattleState BattleContext
initBattleState = StateT $ \s -> return (s, testContext)

-- Uses the State Monad
battleLoop :: BattleState ()
battleLoop = do
    battleState <- get
    let 
        opponentAction = selectOpponentAction (duel battleState) (opponentParty battleState)
        partyHomies = playerParty battleState
        partyHomiesList = partyToList partyHomies
        opponentHomies = opponentParty battleState
        opponentHomiesList = partyToList opponentHomies

    -- Double check that the game is over
    isAnyPartyDead partyHomiesList opponentHomiesList
    updatedState <- get


    if (gameQuit updatedState) 
        then do
            if (winner updatedState)
                then do
                    liftIO $ putStrLn $ "You won the battle!"
                    return ()
                else do
                    liftIO $ putStrLn $ "You lost the battle!"
                    return ()
        else do
            liftIO $ printBattleInfo battleState
            userInput <- fmap (map toLower) $ liftIO getUserInput
            liftIO $ clearScreen -- Clears the terminal
            case userInput of
                "fight" -> do
                    -- Gets the turn order, based on the homie speed, or if the opponent want to do something else than attack
                    turnOrder <- liftIO $ getTurnOrder (duel battleState) opponentAction
                    let homieMoves = moveSetToList $ getHomieMoveSet $ playerHomie $ duel battleState
                    liftIO $ printBattleInfo battleState
                    userMove <- fmap (map toLower) $ liftIO $ getUserMove homieMoves
                    liftIO $ clearScreen -- Clears the terminal
                    -- Turn order selection
                    case turnOrder of

                        Player -> do
                            validMove <- processPlayerMove userMove homieMoves battleState
                            when validMove $ do
                                isAnyPartyDead partyHomiesList opponentHomiesList
                                battleState' <- get
                                opponentDead <- liftIO $ isDuelOpponentDead (duel battleState')
                                if opponentDead && not (gameQuit battleState') then
                                    -- TODO : CALL OPPONENT SWITCH function
                                    liftIO $ putStrLn "Opponent needs to switch."
                                else do
                                    processOpponentMove opponentAction
                                    battleState' <- get
                                    playerDead <- liftIO $ isDuelPlayerDead (duel battleState')

                                    when playerDead $ do
                                        switched <- switchPartyMember
                                        liftIO $ putStrLn ""
                        Opponent -> do
                            processOpponentMove opponentAction
                            battleState' <- get
                            playerDead <- liftIO $ isDuelPlayerDead (duel battleState')

                            unless playerDead $ do
                                validMove <- processPlayerMove userMove homieMoves battleState'
                                when validMove $ do
                                    isAnyPartyDead partyHomiesList opponentHomiesList
                                liftIO $ putStrLn "Player's next actions or state checks here."

                            when playerDead $ do
                                switched <- switchPartyMember
                                liftIO $ putStrLn "Player switched after being defeated."

                    
                    battleState' <- get
                    unless (gameQuit battleState') battleLoop
                    
                "party" -> do
                    switched <- switchPartyMember
                    when switched $ do
                        processOpponentMove opponentAction
                        isAnyPartyDead partyHomiesList opponentHomiesList
                    battleState' <- get
                    playerDead <- liftIO $ isDuelPlayerDead (duel battleState')
                    when playerDead $ do
                        if (gameQuit battleState') then do
                            return ()
                        else do
                            switched <- switchPartyMember
                            liftIO $ putStrLn ""
                    -- liftIO $ putStrLn $ "[DEBUG] \n" <> show (playerParty battleState') <> "\n" <> show (duel battleState') <> "\n\n"
                    unless (gameQuit battleState') battleLoop

                "bag" -> do
                    itemChoice <- liftIO $ getUserItem (playerBag battleState)
                    liftIO $ putStrLn $ itemChoice

                    case itemChoice of
                        "potion" -> do
                            let (homie', bag') = usePotion (playerHomie $ duel battleState) (playerBag battleState)
                                updatedDuel = (duel battleState) { playerHomie = homie' }
                            liftIO $ clearScreen
                            modify $ \s -> s { duel = updatedDuel, playerBag = bag' }
                            liftIO $ putStrLn $ "You used a Potion on " <> homieName (homie')
                            processOpponentMove opponentAction
                            isAnyPartyDead partyHomiesList opponentHomiesList
                            battleState' <- get
                            playerDead <- liftIO $ isDuelPlayerDead (duel battleState')
                            when playerDead $ do
                                if (gameQuit battleState') then do
                                    return ()
                                else do
                                    switched <- switchPartyMember
                                    liftIO $ putStrLn ""
                        "databrus" -> do
                            liftIO $ clearScreen
                            -- Assuming useDataBrus returns updated homie and bag just like usePotion
                            let (homie', bag') = useDataBrus (playerHomie $ duel battleState) (playerBag battleState)
                                updatedDuel = (duel battleState) { playerHomie = homie' }
                            modify $ \s -> s { duel = updatedDuel, playerBag = bag' }
                            liftIO $ putStrLn $ "You used a Databrus on " <> homieName (homie')
                            processOpponentMove opponentAction

                        _ -> do
                            liftIO $ clearScreen
                            liftIO $ putStrLn "Invalid item!"
                            return ()
                    battleLoop

                "quit" -> do
                    modify (\s -> s { gameQuit = True })
                _ -> battleLoop

-- Does the logic for the player Attack
processPlayerMove :: String -> [Move] -> BattleContext -> BattleState Bool
processPlayerMove userMove homieMoves battleState = do
    case readMaybe userMove :: Maybe Int of
        Just index | index > 0 && index <= length homieMoves -> do
            (duel', damageString) <- liftIO $ playerUseMove (index-1) (duel battleState)
            let move = homieMoves !! (index - 1)  -- Convert 1-based index to 0-based
                oppHomie' = opponentHomie duel'
                oppParty' = setPartyMember 0 (Just oppHomie') (opponentParty battleState)
            modify $ \s -> s { duel = duel', opponentParty = oppParty'}
            liftIO $ putStrLn $ "You used: " <> moveName move <> " and did " <> damageString <> " DMG. \n"
            return True
        _ -> do
            liftIO $ putStrLn "Invalid move number. \n"
            return False

switchPartyMember :: BattleState Bool
switchPartyMember = do
    battleState <- get
    let partyHomies = playerParty battleState
    userPartyInput <- liftIO $ fmap (map toLower) $ getUserParty (partyToList partyHomies)

    case readMaybe userPartyInput :: Maybe Int of
        Just index | index > 0 && index <= length (partyToList partyHomies) -> do
            if (index == 1) then do
                liftIO $ clearScreen -- Clears the terminal
                liftIO $ putStrLn "This Homie is already fighting. \n"
                return False
            else do
                let partyHomiesList = partyToList partyHomies
                    currentHomie = playerHomie (duel battleState)
                    homie = partyHomiesList !! (index - 1)
                    party = setPartyMember 0 (Just currentHomie) partyHomies
                    party' = swapPartyOrder 0 (index - 1) party
                    duel' = duel battleState
                    newDuel = duel' { playerHomie = homie }
                --liftIO $ putStrLn $ "[DEBUG] \n" <> show party <> "\n" <> show (duel battleState) <> "\n\n"
                modify $ \s -> s { playerParty = party', duel = newDuel }
                battleState <- get
                --liftIO $ putStrLn $ "[DEBUG] \n" <> show party <> "\n" <> show (duel battleState) <> "\n\n"
                liftIO $ clearScreen -- Clears the terminal
                liftIO $ putStrLn $ "You swapped " <> homieName (partyHomiesList !! 0) <> " and sent out " <> homieName homie <> "\n"
                return True
        _ -> do
                liftIO $ clearScreen -- Clears the terminal
                liftIO $ putStrLn "Invalid index"
                return False


-- Does the logic for the opponent attack
-- TODO: THIS NEEDS TO CHOOSE IF WHAT ACTION HE USES, NOT ONLY ATTACK
processOpponentMove :: OpponentAction -> BattleState ()
processOpponentMove action = case action of
    OpponentAttack -> do
        battleState <- get
        let opponentMoves = moveSetToList $ homieMoves $ opponentHomie $ duel battleState
        moveIndex <- liftIO $ selectOpponentMoveIndex opponentMoves
        (duel', damageString) <- liftIO $ opponentUseMove moveIndex (duel battleState)
        let move = opponentMoves !! moveIndex
            playerHomie' = playerHomie duel'
            playerParty' = setPartyMember 0 (Just playerHomie') (playerParty battleState)
        -- liftIO $ putStrLn $ "PLAYERPARTY:" <> show playerParty' <> "\nDUEL:"<> show duel' <> "\n" <> show playerHomie'
        modify $ \s -> s { duel = duel', playerParty = playerParty' }
        liftIO $ putStrLn $ "Opponent used: " <> moveName move <> " and did " <> damageString <> " DMG. \n"

    OpponentSwitch -> do
        liftIO $ putStrLn "Opponent switches their Homie. \n"

    OpponentBag -> do
        liftIO $ putStrLn "Opponent uses an item from their bag. \n"

isAnyPartyDead :: [Homie] -> [Homie] -> BattleState ()
isAnyPartyDead playerParty opponentParty = do
    let isPlayerDead = isPartyDead playerParty
        isOpponentDead = isPartyDead opponentParty
    if isOpponentDead then do
        modify $ \s -> s { gameQuit = True, winner = True }
    else if isPlayerDead then do
        modify $ \s -> s { gameQuit = True, winner = False }
    else
        return ()

isDuelPlayerDead :: Duel -> IO Bool
isDuelPlayerDead duel = do
    let playerHomie' = playerHomie duel
    if (zeroHPHomie playerHomie') then do
        liftIO $ putStrLn $ "Your, " <> homieName playerHomie' <> " has fainted."
        return True
    else return False

isDuelOpponentDead :: Duel -> IO Bool
isDuelOpponentDead duel = do
    let opponentHomie' = opponentHomie duel
    if (zeroHPHomie opponentHomie') then do
        liftIO $ putStrLn $ "Opponents, " <> homieName opponentHomie' <> " has fainted."
        return True
    else return False

getTurnOrder :: Duel -> OpponentAction -> IO TurnOrder
getTurnOrder duel opponentAction = do
    let playerSpeed = getHomieSpd (playerHomie duel)
        opponentSpeed = getHomieSpd (opponentHomie duel)
    if opponentAction /= OpponentAttack
        then return Opponent
        else case compare playerSpeed opponentSpeed of
            GT -> return Player
            LT -> return Opponent
            EQ -> do
                result <- randomRIO (0, 1) :: IO Int
                return $ if result == 0 then Player else Opponent

getUserInput :: IO String
getUserInput = do
    putStrLn "\n Select an action \nfight | party | bag"
    getLine

getUserMove :: [Move] -> IO String
getUserMove moves = do
    let movesDisplay = unlines $ zipWith (\index move -> show index ++ ": " ++ show move) [1..] moves
    putStrLn $ "\nUse index to select your move \n" <> movesDisplay
    getLine

getUserItem :: Bag -> IO String
getUserItem bag = do
    putStrLn $ "choose an item (write item name):\n to return simply input anything else\n"
    putStrLn $ "potion: " <> show (potions bag) <> ""
    putStrLn $ "databrus: " <> show (databrus bag) <> "\n"
    getLine

moveSetToList :: MoveSet -> [Move]
moveSetToList (MoveSet m1 m2 m3 m4) = catMaybes [m1, m2, m3, m4]

getUserParty :: [Homie] -> IO String
getUserParty (x:xs) = do
    let partyDisplay = unlines $ zipWith (\index homie -> show index ++ ": " ++ show homie ++ homieDeadString homie) [2..] xs
    putStrLn $ "Use index to choose!\n" <>"1: " <> show x <> " [Current]"<> homieDeadString x <>"\n" <> partyDisplay
    putStrLn $ "to return simply input anything else\n"
    getLine

-- Prints HP of your Homie, and the enemy Homie
-- Maybe add ASCII Art for Homies?
printBattleInfo :: BattleContext -> IO ()
printBattleInfo context = do
    let duelVar = duel context
        player = playerHomie duelVar
        opponent = opponentHomie duelVar
    putStrLn $ "Player: \n" ++ show player
    putStrLn $ "\nOpponent: \n" ++ show opponent