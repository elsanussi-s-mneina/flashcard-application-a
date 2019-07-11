module Lib
    ( welcome
    ) where

-- | Print a welcome message for the student to see.
welcome :: IO ()
welcome = putStrLn "Welcome to Remember the Letter (Haskell)"
