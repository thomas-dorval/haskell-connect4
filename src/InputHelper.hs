module InputHelper (inputHelp, inputHelpReturn) where

import Data.List (elemIndex)

-- InputHelper is a module to handle user input and output due to repeated use of similar functions across the application.

-- InputHelp is a function that will take a string, an Integer for the number of options, 
-- an array of characters for those options, and an array of functions correlating to those options,
-- and will prompt the user for input until a valid input is given, at which point it will run the function corresponding to that input.
inputHelp :: String -> [Char] -> [IO ()] -> IO ()
inputHelp prompt options functions = do
        putStrLn prompt
        ans <- getLine
        case ans of
            [c] ->
                case elemIndex c options of
                    Just index -> functions !! index
                    Nothing -> invalid
            _ -> invalid
      where
        invalid = do
            putStrLn "Please enter a valid answer!"
            inputHelp prompt options functions

-- inputHelp with return functionality
inputHelpReturn :: String -> [Char] -> [IO a] -> IO a
inputHelpReturn prompt options actions = do
        putStrLn prompt
        ans <- getLine
        case ans of
            [c] ->
                case elemIndex c options of
                    Just index -> actions !! index
                    Nothing -> invalid
            _ -> invalid
      where
        invalid = do
            putStrLn "Please enter a valid answer!"
            inputHelpReturn prompt options actions