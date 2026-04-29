{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Check where

import Board

-- Returns a boolean indicating if the game has been won and a Chip indicating the winner (Red or Yellow) if there is one.
checkAllWin :: Board -> (Bool, Chip)
checkAllWin board =
    let (redWin, _) = checkWin board Red 4
        (yellowWin, _) = checkWin board Yellow 4
    in if redWin then (True, Red)
       else if yellowWin then (True, Yellow)
       else (False, Empty)

-- Check if the game has been won by checking for Int in a row in optimal directions.
-- For distance=4, returns (True, Nothing) if a win is found, else (False, Nothing).
-- For distance=3, returns (True, Just emptyPos) if 3 in a row with empty 4th is found, else (False, Nothing).
checkWin :: Board -> Chip -> Int -> (Bool, Maybe (Int, Int))
checkWin (Board grid) chip distance = 
  let results = concat [map (checkLine (Board grid) chip distance dir) allPositions | dir <- directions]
      successResults = filter fst results
  in case successResults of
      [] -> (False, Nothing)
      ((_, m):_) -> (True, m)
  where
    directions = [(-1, 0), (1, 0), (0, 1), (1, 1), (1, -1)] -- right, down, down-right, down-left
    allPositions = case grid of
      [] -> []
      (firstRow:_) -> [(x, y) | x <- [0..length grid - 1], y <- [0..length firstRow - 1]] -- all positions in the grid

-- Check for Int in a row starting from a specific position in a specific direction.
-- For distance=4, returns (True, Nothing) if 4 in a row is found.
-- For distance=3, returns (True, Just emptyPos) if 3 in a row with empty 4th is found, else (False, Nothing).
checkLine :: Board -> Chip -> Int -> (Int, Int) -> (Int, Int) -> (Bool, Maybe (Int, Int))
checkLine (Board grid) chip distance (dx, dy) (x, y) =
  if distance == 4 
    then let chipLine = [getChip (Board grid) (x + i * dx, y + i * dy) | i <- [0..distance - 1]]
         in (all (== chip) chipLine, Nothing)
    else let chipLine = [getChip (Board grid) (x + i * dx, y + i * dy) | i <- [0..distance - 1]]
             emptyPos = (x + distance * dx, y + distance * dy)
             emptyChip = getChip (Board grid) emptyPos
         in if all (== chip) chipLine && emptyChip == Empty
            then let (ex, ey) = emptyPos --check if emptyPos has a chip underneath it or is on the bottom row
                     belowPos = (ex, ey - 1)
                 in if ey == 0 || getChip (Board grid) belowPos /= Empty
                    then (True, Just emptyPos)
                    else (False, Nothing)
             else (False, Nothing)

-- Get the chip at a specific position, returning Empty if out of bounds.
getChip :: Board -> (Int, Int) -> Chip
getChip (Board grid) (x, y)
    | x < 0 || x >= length grid = Empty
    | otherwise = case grid !! x of
        row | y < 0 || y >= length row -> Empty
            | otherwise -> row !! y