module TowersOfHanoiSpec where

import Test.Hspec
import TowersOfHanoi

main :: IO ()
main = hspec $
  describe "Towers of Hanoi solver" $ do
    it "should be able to solve 2 correctly" $ do
      let expectedMoves = [('a', 'c'), ('a', 'b'), ('c', 'b')]
      hanoi 2 'a' 'b' 'c' `shouldBe` expectedMoves

    it "should be able to solve 3 correctly" $ do
      let expectedMoves = [('a','b'),('a','c'),('b','c'),('a','b')
                          ,('c','a'),('c','b'),('a','b')]
      hanoi 3 'a' 'b' 'c' `shouldBe` expectedMoves
