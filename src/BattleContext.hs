module BattleContext where

import Duel
import Party
import Opponent
import Homie
import Moves
import Utils
import Stats
import Control.Monad.State
import Data.Char (toLower)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

data BattleContext = BattleContext { duel :: Duel, playerParty :: Party, opponentParty :: Party, opponentType :: AIBehaviour, gameQuit :: Bool}

data TurnOrder = Player | Equal | Opponent
    deriving (Show, Eq)

type BattleState = StateT BattleContext IO

initContext = BattleContext {duel = testDuel, playerParty = testParty, opponentParty = testParty, opponentType = Normal, gameQuit = False}

initBattleState :: BattleState BattleContext
initBattleState = StateT $ \s -> return (s, initContext)

-- Uses the State Monad
battleLoop :: BattleState ()
battleLoop = do
    battleState <- get
    if checkForEndOfBattle battleState
        then return () -- Battle over, stop gameloop
        else do
            liftIO $ printBattleInfo battleState
            userInput <- fmap (map toLower) $ liftIO getUserInput
            liftIO $ clearScreen -- Clears the terminal
            case userInput of
                "fight" -> do
                    -- Gets the turn order, based on the homie speed
                    turnOrder <- liftIO $ getTurnOrder $ duel battleState
                    let homieMoves = moveSetToList $ getHomieMoveSet $ playerHomie $ duel battleState
                    liftIO $ printBattleInfo battleState
                    userMove <- fmap (map toLower) $ liftIO $ getUserMove homieMoves
                    liftIO $ clearScreen -- Clears the terminal
                    
                    -- Turn order selection
                    case turnOrder of
                        Player -> do
                            processPlayerMove userMove homieMoves battleState
                            get >>= processOpponentMove

                        Opponent -> do
                            processOpponentMove battleState
                            get >>= processPlayerMove userMove homieMoves
                    
                    liftIO $ putStrLn ""
                    return ()
                    
                "party" -> do
                    userParty <- fmap (map toLower) $ liftIO getUserParty
                    liftIO $ clearScreen -- Clears the terminal

                    case userParty of
                        "1" -> do
                            liftIO $ putStrLn "You switched to 1"
                            return ()
                        _ -> return ()
                    return ()
                "quit" -> do
                    modify (\s -> s { gameQuit = True })
                _ -> return ()

            battleLoop -- Continue the loop

-- Does the logic for the player Attack
processPlayerMove :: String -> [Move] -> BattleContext -> BattleState ()
processPlayerMove userMove homieMoves battleState = do
    case readMaybe userMove :: Maybe Int of
        Just index | index > 0 && index <= length homieMoves -> do
            let move = homieMoves !! (index - 1)  -- Convert 1-based index to 0-based
                (duel', damageString) = playerUseMove (index-1) (duel battleState)
            modify $ \s -> s { duel = duel' }
            liftIO $ putStrLn $ "You used: " <> moveName move <> " and did " <> damageString <> " DMG."
        _ -> liftIO $ putStrLn "Invalid move number."

-- Does the logic for the opponent attack
processOpponentMove :: BattleContext -> BattleState ()
processOpponentMove battleState = do
    let opponentMoves = moveSetToList $ homieMoves $ opponentHomie $ duel battleState
    moveIndex <- liftIO $ selectOpponentMoveIndex opponentMoves
    let move = opponentMoves !! moveIndex
        (duel', damageString) = opponentUseMove moveIndex (duel battleState)
    modify $ \s -> s { duel = duel' }
    liftIO $ putStrLn $ "Opponent used: " <> moveName move <> " and did " <> damageString <> " DMG."

-- End of Battle Criterion
checkForEndOfBattle :: BattleContext -> Bool
checkForEndOfBattle context = gameQuit context 

getTurnOrder :: Duel -> IO TurnOrder
getTurnOrder duel = do
    let playerSpeed = getHomieSpd (playerHomie duel)
        opponentSpeed = getHomieSpd (opponentHomie duel)
    case compare playerSpeed opponentSpeed of
        GT -> return Player
        LT -> return Opponent
        EQ -> do
            result <- randomRIO (0, 1) :: IO Int
            return $ if result == 0 then Player else Opponent

getUserInput :: IO String
getUserInput = do
    putStrLn "\n Select an action \nFight | Party | Bag"
    getLine

getUserMove :: [Move] -> IO String
getUserMove moves = do
    let movesDisplay = unlines $ zipWith (\index move -> show index ++ ": " ++ show move) [1..] moves
    putStrLn $ "\nSelect your move \n" <> movesDisplay <> "'return' to go back to the previous menu"
    getLine

moveSetToList :: MoveSet -> [Move]
moveSetToList (MoveSet m1 m2 m3 m4) = catMaybes [m1, m2, m3, m4]

-- TODO : LOAD HOMIES FROM PARTY
getUserParty :: IO String
getUserParty = do
    putStrLn "pick X"
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