module BowlingGameSpec where

import BowlingGame
import Test.Hspec

strike :: Int
strike = 10

maxNumOfRolls :: Int
maxNumOfRolls = 20

main :: IO ()
main = hspec $
  describe "Bowling Game" $
    describe "score method" $ do
      it "returns a score of 0 when given a gutter game" $ do
        let gutterBall = 0 :: Int
        scoreGame (replicate maxNumOfRolls gutterBall) `shouldBe` 0
      it "returns a score of 20 when given a game of all ones" $
        scoreGame (replicate maxNumOfRolls 1) `shouldBe` (20 :: Int)
      it "can score a single spare followed by gutter balls" $ do
        let rollsRemaining = 17 :: Int
        scoreGame (5 : 5 : 3 : replicate rollsRemaining 0) `shouldBe` (16 :: Int)
      it "can score a single strike followed by gutter balls" $ do
        let rollsRemaining = 16 :: Int
        scoreGame (strike : 3 : 4 : replicate rollsRemaining 0) `shouldBe` (24 :: Int)
      it "can score a perfect game" $ do
        let rollsRemaining = 12 :: Int
        scoreGame (replicate rollsRemaining strike) `shouldBe` (300 :: Int)
      it "can score some acceptance tests" $ do
        scoreGame [5,2,3,4,7,3,1,9,10,5,3,10,8,2,6,0,8,2,7] `shouldBe` (130 :: Int)
        scoreGame [5,2,3,4,7,3,8,2,10,5,3,10,8,2,6,0,8,2,7] `shouldBe` (137 :: Int)
        scoreGame [10,10,8,0,10,10,10,10,7,0,10,10,10,10] `shouldBe` (225 :: Int)
