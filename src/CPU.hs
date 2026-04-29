module CPU (cpuLogic) where

import Board
import Check

cpuLogic :: Game -> (IO Game, Int)
cpuLogic (Game (board, turn)) =
    let turnColor = if even turn then Red else Yellow
        Board grid = board
        half = length grid `div` 2
    in if turn < 2 && getChip board (half, 0) == Empty
            then (placeChipInBoard (Game (board, turn)) half, half + 1)
            else checkForWinLose (Game (board, turn)) turnColor

checkForWinLose :: Game -> Chip -> (IO Game, Int)
checkForWinLose (Game (board, turn)) turnColor =
    let (_, maybePos) = checkWin board turnColor 3
    in case maybePos of
        Just (x, _) -> (placeChipInBoard (Game (board, turn)) x, x + 1)
        Nothing -> 
            let (_, maybeBlockPos) = checkWin board (Board.not turnColor) 3
            in case maybeBlockPos of
                Just (x, _) -> (placeChipInBoard (Game (board, turn)) x, x + 1)
                Nothing -> 
                    let col = randomValidColumn board
                    in (placeChipInBoard (Game (board, turn)) col, col + 1)

randomValidColumn :: Board -> Int
randomValidColumn (Board grid) = uncons [i + 1 | i <- [0..length grid - 1], Prelude.not (isColumnFull (Board grid) i)]
  where
    uncons (x:_) = x
    uncons [] = error "No valid columns available"