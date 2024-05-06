module Utils where

import System.Info (os)
import System.Process (callCommand)

clearScreen :: IO ()
clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"