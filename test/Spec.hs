module Main(main) where

import Board
import CPU
import Test.Hspec -- visual studio is lying this is not a missing module or Spec would be throwing an error


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "CPU Logic" $ do
        it "should place a chip in the middle column on the first turn" $ do
            let game = Game (Board (replicate 7 (replicate 6 Empty)), 0)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should place a chip in the middle column on the second turn" $ do
            let game = Game (Board (replicate 7 (replicate 6 Empty)), 1)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should take a vertical winning move" $ do
            let board = Board [ replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , [Yellow, Yellow, Yellow, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should take a horizontal winning move" $ do
            let board = Board [ [Yellow, Empty, Empty, Empty, Empty, Empty]
                              , [Yellow, Empty, Empty, Empty, Empty, Empty]
                              , [Yellow, Empty, Empty, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should block a vertical opponent winning move" $ do
            let board = Board [ replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , [Red, Red, Red, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should block a horizontal opponent winning move" $ do
            let board = Board [ [Red, Empty, Empty, Empty, Empty, Empty]
                              , [Red, Empty, Empty, Empty, Empty, Empty]
                              , [Red, Empty, Empty, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should block a horizontal opponent winning move from the edge column" $ do
            let board = Board [ [Yellow, Yellow, Empty, Empty, Empty, Empty]
                              , replicate 6 Empty 
                              , replicate 6 Empty
                              , [Red, Empty, Empty, Empty, Empty, Empty]
                              , [Red, Empty, Empty, Empty, Empty, Empty]
                              , [Red, Empty, Empty, Empty, Empty, Empty]
                              , replicate 6 Empty ]
            let game = Game (board, 5)
            let (_, col) = cpuLogic game
            col `shouldBe` 7

        it "should take a diagonal winning move" $ do
            let board = Board [ [Yellow, Empty, Empty, Empty, Empty, Empty]
                              , [Red, Yellow, Empty, Empty, Empty, Empty]
                              , [Red, Red, Yellow, Empty, Empty, Empty]
                              , [Red, Red, Red, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should block a diagonal opponent winning move" $ do
            let board = Board [ [Red, Empty, Empty, Empty, Empty, Empty]
                              , [Red, Red, Empty, Empty, Empty, Empty]
                              , [Yellow, Yellow, Red, Empty, Empty, Empty]
                              , [Yellow, Yellow, Yellow, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 3)
            let (_, col) = cpuLogic game
            col `shouldBe` 4

        it "should take winning move over blocking move if both are available" $ do
            let board = Board [ replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , [Yellow, Yellow, Yellow, Empty, Empty, Empty]
                              , [Red, Red, Red, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 5)
            let (_, col) = cpuLogic game
            col `shouldBe` 5

        it "should take winning move over blocking move if both are available even if blocking move is in the middle column" $ do
            let board = Board [ replicate 6 Empty
                              , replicate 6 Empty
                              , replicate 6 Empty
                              , [Red, Red, Red, Empty, Empty, Empty]
                              , [Yellow, Yellow, Yellow, Empty, Empty, Empty]
                              , replicate 6 Empty
                              , replicate 6 Empty ]
            let game = Game (board, 5)
            let (_, col) = cpuLogic game
            col `shouldBe` 5