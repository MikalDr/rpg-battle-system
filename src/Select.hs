module Select where

import Types

displayStartHomies :: [Homie] -> IO ()
displayStartHomies [x, y, z] = do
    putStrLn $ "1: " <> show x <> " " <> show (homieType x) <> "\n" <> "   " <> show (homieStats x)
    putStrLn $ "2: " <> show y <> " " <> show (homieType y) <> "\n" <> "   " <> show (homieStats y)
    putStrLn $ "3: " <> show z <> " " <> show (homieType z) <> "\n" <> "   " <> show (homieStats z)
