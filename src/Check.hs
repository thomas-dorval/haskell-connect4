module Check where

import Board

checkAllWin :: Board -> Bool
checkAllWin board = any (checkWin board) [Red, Yellow]

-- Check if the game has been won by checking for 4 in a row in all directions.
checkWin :: Board -> Chip -> Bool
checkWin (Board grid) chip = any (checkDirection grid chip) directions
  where
    directions = [(1, 0), (0, 1), (1, 1), (1, -1)] -- right, down, down-right, down-left

-- Check for 4 in a row in a specific direction.
checkDirection :: [[Chip]] -> Chip -> (Int, Int) -> Bool
checkDirection grid chip (dx, dy) = any (checkLine grid chip (dx, dy)) allPositions
  where
    allPositions = [(x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1]]

-- Check for 4 in a row starting from a specific position in a specific direction.
checkLine :: [[Chip]] -> Chip -> (Int, Int) -> (Int, Int) -> Bool
checkLine grid chip (dx, dy) (x, y) =
    all (== chip) [getChip grid (x + i * dx, y + i * dy) | i <- [0..3]]

-- Get the chip at a specific position, returning Empty if out of bounds.
getChip :: [[Chip]] -> (Int, Int) -> Chip
getChip grid (x, y)
    | x < 0 || x >= length grid || y < 0 || y >= length (head grid) = Empty
    | otherwise = (grid !! x) !! y