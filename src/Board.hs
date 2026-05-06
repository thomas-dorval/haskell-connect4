{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Board where

import Prelude hiding (not)
import InputHelper (inputHelpReturn)

class Not a where
    not :: a -> a

data Chip = Red | Yellow | Empty
    deriving (Eq, Enum, Bounded)

-- The Board type is a 2D list representing the grid of chips, where each array is a column.
newtype Board = Board [[Chip]]

-- The Game type is a tuple where the first element is a Board, and the second element is an integer representing the turn of the game.
newtype Game = Game (Board, Int) 
-- | E | E | E | E | E | E | E |
-- | E | E | E | E | E | E | E |
-- | E | E | E | E | E | E | E |
-- | E | E | E | E | E | E | E |
-- | E | E | E | E | E | E | E |
-- | E | E | E | E | E | E | E |
--   1   2   3   4   5   6   7

instance Show Chip where
    show Red = "R"
    show Yellow = "Y"
    show Empty = " "

instance Not Chip where
    not Red = Yellow
    not Yellow = Red
    not Empty = Empty

instance Show Board where
    show (Board board) = concatMap showColumn (reverse (transpose board)) ++ "\n" ++ showIndices ++ "\n"
      where
        showColumn col = "|" ++ concatMap (\chip -> " " ++ show chip ++ " |") col ++ "\n"
        transpose = foldr (zipWith (:)) (repeat [])
        showIndices = "  " ++ concatMap (\i -> show i ++ "   ") [1..length board]

instance Show Game where
    show (Game (board, turn)) = show board ++ "Turn: " ++ show turn

-- Prompt user for whether they want to play with the standard Connect-4 board or a custom sized board; redirect to proper functions.
promptBoardType :: IO Board
promptBoardType = 
    inputHelpReturn "Do you want to play with the default board (7 horizontal x 6 vertical) or a custom board?\n\t1. Default Board\n\t2. Custom Board"
                      ['1', '2']
                      [return $ Board (boardInit (7, 6)), promptCustomBoard]

-- Ask user for custom board dimensions, where x must be greater than or equal to y, must be positive integers, and cannot be greater than 10.
promptCustomBoard :: IO Board
promptCustomBoard = do
    putStrLn "Please enter the board dimensions in '(x,y)' format. Boards must have x >= y and must be between 4 and 10: "
    ans <- getLine
    let (x, y) = parseDimensions ans
    if x >= y && x >= 4 && y >= 4 && x <= 10 && y <= 10
        then return $ Board (boardInit (x, y))
        else do
            putStrLn $ "Invalid dimensions: " ++ show x ++ " " ++ show y ++ ". Please try again."
            promptCustomBoard

-- Parse the dimensions from the user input string above.
parseDimensions :: String -> (Int, Int)
parseDimensions s =
    let trimmed = dropWhile (\c -> c == '(' || c == ' ') s
        noEnd = takeWhile (/= ')') trimmed
        (xStr, rest) = break (== ',') noEnd
        yStr = dropWhile (\c -> c == ',' || c == ' ') rest
    in (read xStr, read yStr)

-- Create a Board of Empty Chips which is an array of X arrays of size Y where X and Y are the pair of integers provided.
boardInit :: (Int, Int) -> [[Chip]]
boardInit (x, y) = replicate x (replicate y Empty)

-- Check if the column is full by checking if the topmost cell in the column is not Empty.
isColumnFull :: Board -> Int -> Bool
isColumnFull (Board grid) col = (grid !! col) !! (length (grid !! col) - 1) /= Empty

-- Check if board is full by checking if all columns are full.
isBoardFull :: Board -> Bool
isBoardFull board = all (isColumnFull board) [0..length (let Board grid = board in grid) - 1]

-- Place a chip in the specified column based on the current turn where Red is even and Yellow is odd, and return the new game state. 
-- If the column is full, inform the user to choose another column.
placeChipInBoard :: Game -> Int -> IO Game
placeChipInBoard (Game (board, turn)) col
    | col < 1 || col > length (let Board grid = board in grid) = do
        putStrLn $ "Invalid column: " ++ show col ++ ". Please enter a valid column number."
        return (Game (board, turn))
    | isColumnFull board (col - 1) = do
        putStrLn $ "Column " ++ show col ++ " is full. Please choose another column."
        return (Game (board, turn))
    | otherwise = do
        let chip = if even turn then Red else Yellow
            newBoard = placeChipInColumn board (col - 1) chip
        return (Game (newBoard, turn + 1))
    
-- Place a chip in the specified column and return the new board state.
placeChipInColumn :: Board -> Int -> Chip -> Board
placeChipInColumn (Board board) col chip =
    let column = board !! col
        newColumn = placeChip column chip
    in Board (take col board ++ [newColumn] ++ drop (col + 1) board)

-- Place a chip in the column by replacing the first Empty cell with the chip.
placeChip :: [Chip] -> Chip -> [Chip]
placeChip [] _ = []
placeChip (Empty:rest) chip = chip : rest
placeChip (c:rest) chip = c : placeChip rest chip