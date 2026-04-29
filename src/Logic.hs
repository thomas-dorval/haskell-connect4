module Logic (currentGameState) where

import Board
import Check
import InputHelper (inputHelpReturn, inputHelp)
import CPU (cpuLogic)

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
currentTurn (Game(board, turn), isComputer) = do
    if isBoardFull board
        then inputHelp "The game is a draw!\nDo you want to play again? (y/n)"
                       ['y', 'n']
                       [currentGameState, putStrLn "Thanks for playing!"]
        else do
            let (hasWon, winner) = checkAllWin board
            if Prelude.not hasWon
                then do
                    currentGame <- playerTurn (Game(board, turn), isComputer)
                    currentTurn currentGame
            else do
                print board
                inputHelp ("Player " ++ show winner ++ " wins!\nDo you want to play again? (y/n)")
                    ['y', 'n']
                    [currentGameState, putStrLn "Thanks for playing!"]

-- Ask user for number of players and if going first or second
playerCheck :: IO GameState
playerCheck =
    inputHelpReturn "Do you want to play against a computer or another player?\n\t1. Another Player\n\t2. Computer"
                      ['1', '2']
                      [do board <- promptBoardType
                          return (Game (board, 0), False),
                       do board <- promptBoardType
                          inputHelpReturn
                            "Do you want to go first or second?\n\t1. First\n\t2. Second"
                            ['1', '2']
                            [return (Game (board, 0), True), return (Game (board, 1), True)]]

-- Inform player of current turn, and if non-computer player, ask for column input to run placeChipInBoard; repeat until valid input is given
playerTurn :: GameState -> IO GameState
playerTurn (game@(Game (board, turn)), isComputer) = do
    putStrLn $ "Current board:\n" ++ show board
    putStrLn $ "Player " ++ show (if even turn then "Red" else "Yellow") ++ "'s turn."
    if isComputer && odd turn
        then do
            -- Computer's turn: implement a simple strategy (e.g., random valid move)
            let (newGameIO, col) = cpuLogic game
            putStrLn $ "Computer chooses column " ++ show col ++ "."
            newGame <- newGameIO
            return (newGame, isComputer)
        else do
            -- Player's turn: ask for column input
            putStrLn "Please enter the column number to place your chip: "
            colInput <- getLine
            case reads colInput :: [(Int, String)] of
                [(col, "")] -> do
                    newGame <- placeChipInBoard game col
                    return (newGame, isComputer)
                _ -> do
                    putStrLn $ "Invalid input: " ++ colInput ++ ". Please enter a valid column number."
                    playerTurn (game, isComputer)