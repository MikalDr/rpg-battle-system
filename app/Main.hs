module Main (main) where

import Lib
import BattleContext
import Control.Monad.Trans.State (execStateT, runStateT, evalStateT)
import Party
import Types
import Bag
import Utils
import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Select
import Homie
import Opponent
import Text.Read (readMaybe)

data GameState = Start | Select | Fight | Reward | Planning | Exit
    deriving (Show, Eq)

data GameContext = GameContext {
    currentState :: GameState,
    gameParty :: Party,
    inventory :: Bag,
    startHomies :: [Homie]
}
    deriving (Show)

initGame :: GameContext
initGame = GameContext {
    currentState = Start, 
    gameParty = initParty, 
    inventory = startBag,
    startHomies = [testHomie, testHomie2, testHomie3]
    }


asciiLogo :: String
asciiLogo = unlines [
    " _   _                    _       ______  _         _      _   ",
    "| | | |                  (_)      |  ___|(_)       | |    | |  ",
    "| |_| |  ___   _ __ ___   _   ___ | |_    _   __ _ | |__  | |_ ",
    "|  _  | / _ \\ | '_ ` _ \\ | | / _ \\|  _|  | | / _` || '_ \\ | __|",
    "| | | || (_) || | | | | || ||  __/| |    | || (_| || | | || |_ ",
    "\\_| |_/ \\___/ |_| |_| |_||_| \\___|\\_|    |_| \\__, ||_| |_| \\__|",
    "                                              __/ |            ",
    "                                             |___/             "
    ]

main :: IO ()
main = do
    let context = initGame
    mainLoop context

mainLoop :: GameContext -> IO ()
mainLoop context = do
    unless (currentState context == Exit) $ do
        newContext <- transitionState context
        mainLoop newContext

transitionState :: GameContext -> IO GameContext
transitionState context = case currentState context of
    Start -> do
        clearScreen
        putStrLn asciiLogo
        putStrLn "\nWelcome, write 'start' to start the game"
        userInput <- getLine
        return $ if userInput == "start"
                 then context { currentState = Select }
                 else context

    Select -> do
        clearScreen
        putStrLn "Select your first Homie! (use the indexes)"
        displayStartHomies starters
        userInput <- getLine
        case userInput of
            "1" -> selectOption 0
            "2" -> selectOption 1
            "3" -> selectOption 2
            _   -> return context  -- Return the same context to retry selection
        where
            starters = startHomies context
            myParty = gameParty context
            selectOption idx = do
                clearScreen
                putStrLn $ "You chose " <> homieName (starters !! idx) <> "\nAre you sure, if so let us commence battle?\nyes/no"
                battleReady <- getLine
                case battleReady of
                    "yes" -> do
                        if (length (partyToList $ myParty) >= 1) then do
                            return context { currentState = Planning, gameParty = setPartyMember (partySize myParty) (Just (starters !! idx)) myParty }
                        else do
                            return context { currentState = Fight, gameParty = setPartyMember (partySize myParty) (Just (starters !! idx)) myParty }
                    _ -> return context

    Fight -> do
        let myParty = gameParty context
            myBag = inventory context
        clearScreen
        (_, finalState) <- runStateT battleLoop (initContext myParty myParty myBag)
        return context { currentState = Reward, gameParty = (playerParty finalState), inventory = (playerBag finalState) }

    Reward -> do
        let myParty = gameParty context
            myPartyList = partyToList myParty
            myBag = inventory context
        clearScreen
        putStrLn "Choose what kind of reward you want:\nlevelup | homie | item"
        userInput <- getLine
        case userInput of
            "levelup" -> do
                clearScreen
                putStrLn "Choose who to level up"
                userPartyInput <- getUserParty myPartyList
                case readMaybe userPartyInput :: Maybe Int of
                    Just index | index > 0 && index <= length myPartyList -> do
                        homie' <- levelUp( unsafePartyConvert $ getPartyMember (index-1) myParty)
                        let myParty' = setPartyMember (index-1) (Just homie') myParty
                        let context' = context { gameParty = myParty', currentState=Planning }
                        return context'
                    _ -> do
                        clearScreen -- Clears the terminal
                        putStrLn "Invalid index"
                        return context { currentState = Reward}

            "homie" -> return context { currentState = Select }
            "item" -> return context { currentState = Planning, inventory = (addPotion (inventory context) 1) }
            "exit" -> return context { currentState = Exit }
            _ -> return context { currentState = Reward}

    Planning -> do
        let myParty = gameParty context
            myPartyList = partyToList myParty
            myBag = inventory context
        clearScreen
        putStrLn "Swap Party Order! \n'x y' where the variables are the indices \n\nwhen you are ready to continue, use 'ready' to enter fight \n\n"
        userInput <- getUserParty myPartyList
        case userInput of
            "ready" -> return context { currentState = Fight }
            _ -> do 
                let indices = parseIndices userInput
                case indices of
                    [i, j] | validIndex (i-1) myPartyList && validIndex (j-1) myPartyList -> do
                        let newParty = swapPartyOrder (i-1) (j-1) myParty
                        return context { gameParty = newParty, currentState = Planning}
                    _ -> return context { currentState = Planning }

parseIndices :: String -> [Int]
parseIndices input = catMaybes $ map readMaybe $ words input

validIndex :: Int -> [a] -> Bool
validIndex idx list = idx >= 0 && idx < length list