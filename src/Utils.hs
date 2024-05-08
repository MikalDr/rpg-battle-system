module Utils where

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J"