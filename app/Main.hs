module Main (main) where

import Logic (currentGameState)
import InputHelper (inputHelp)

main :: IO ()
main = do
  putStrLn "==============================\n\tHaskell Connect-4 Project\n\tBy Thomas Dorval\n==============================\n"
  currentGameState
  inputHelp "Are you sure you want to exit? (y/n)"
            ['y', 'n']
            [putStrLn "Exiting...", main]