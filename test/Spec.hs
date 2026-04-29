module Main where

import Board
import CPU
import Logic
import Test.Hspec -- visual studio is lying this is not a missing module or Spec would be throwing an error


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "CPU Logic" $ do
        it "should place a chip in the middle column on the first turn" $ do
            let game = Game (Board (replicate 6 (replicate 7 Empty)), 0)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should place a chip in the middle column on the second turn" $ do
            let game = Game (Board (replicate 6 (replicate 7 Empty)), 1)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should block opponent's winning move" $ do
            let board = Board [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Red,   Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Red,   Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Red,   Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Yellow,Empty, Empty, Empty] ]
            let game = Game (board, 2)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should take winning move if available" $ do
            let board = Board [ [Empty, Empty, Empty, Red,   Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Red,   Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Red,   Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Yellow,Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Yellow,Empty, Empty, Empty]
                              , [Empty, Empty, Empty,Yellow ,Empty ,Empty ,Empty ] ]
            let game = Game (board ,3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should place a chip in a random valid column if no winning or blocking move is available" $ do
            let board = Board [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                              , [Empty, Empty, Empty, Empty, Empty, Empty, Empty] ]
            let game = Game (board, 4)
            let (_, col) = cpuLogic game
            col `shouldSatisfy` (\c -> c >= 1 && c <= 7)