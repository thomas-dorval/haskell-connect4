module Logic where

import Board
import Check

-- Container for game where bool is true if the game is against a computer or false if not.
type GameState = (Game, Bool)

-- Ask user for number of players
-- If computer, ask for going first or second
-- Ask user for board type
-- Inform user of player turn, and for column input, repeat until valid input is given
-- After placing valid input check for win
-- Wait for player 2 input or computer input depending on number of players
-- Check for win
-- Repeat until win or draw
-- Ask user if they want to play again, if so loop, if not exit
currentGameState :: IO ()
currentGameState = do 
    currentGame <- playerCheck
    currentTurn currentGame

currentTurn :: GameState -> IO ()
currentTurn (Game(board, turn), isComputer) = 
    if not (checkAllWin board)
        then do
            currentGame <- playerTurn (Game(board, turn), isComputer)
            currentTurn currentGame
        else do
            putStrLn "Game over! Do you want to play again? (y/n)"
            ans <- getLine
            case ans of
                "y" -> currentGameState
                _ -> putStrLn "Thanks for playing!"

-- Ask user for number of players and if going first or second
playerCheck :: IO GameState
playerCheck = do
    putStrLn "Do you want to play against a computer or another player?\n\t1. Another Player\n\t2. Computer\n"
    ans <- getLine
    case ans of
        "1" -> do
            board <- promptBoardType
            return (Game (board, 0), False) -- Two players, so turn starts at 0
        "2" -> do
            putStrLn "Computer player WIP."
            playerCheck
        --    putStrLn "Do you want to go first or second?\n\t1. First\n\t2. Second\n"
        --    orderAns <- getLine
        --    case orderAns of
        --        "1" -> return (Game (promptBoardType, 0), True) -- Player goes first, so turn starts at 0
        --        _ -> do
        --            putStrLn "Please enter a valid answer!"
        --            playerCheck
        _ -> do
            putStrLn "Please enter a valid answer!"
            playerCheck

-- Inform player of current turn, and if non-computer player, ask for column input to run placeChipInBoard; repeat until valid input is given
playerTurn :: GameState -> IO GameState
playerTurn (game@(Game (board, turn)), isComputer) = do
    putStrLn $ "Current board:\n" ++ show board
    putStrLn $ "Player " ++ show (if even turn then "Red" else "Yellow") ++ "'s turn."
    if isComputer && odd turn
        then do
            -- Computer's turn: implement a simple strategy (e.g., random valid move)
            --let col = chooseComputerMove board
            --putStrLn $ "Computer chooses column: " ++ show col
            --newGame <- placeChipInBoard game col
            --return (newGame, isComputer)
            return (game, isComputer) -- Placeholder for computer move logic
        else do
            -- Player's turn: ask for column input
            putStrLn "Please enter the column number to place your chip: "
            colInput <- getLine
            case reads colInput :: [(Int, String)] of
                [(col, "")] -> do
                    newGame <- placeChipInBoard game col
                    return (newGame, isComputer)
                _ -> do
                    putStrLn "Invalid input. Please enter a valid column number."
                    playerTurn (game, isComputer)