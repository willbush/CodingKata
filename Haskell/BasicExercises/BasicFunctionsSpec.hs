module ToySpec where

import Test.Hspec
import Toy

main :: IO ()
main = hspec $ do
  describe "reverse" $
    it "should reverses a list" $
      reverse' ["a","b","c","d"] `shouldBe` ["d","c","b","a"]

  describe "map" $
    it "should a function over a list" $
      map' (+ 1) ([1,2,3,4] :: [Int]) `shouldBe` [2,3,4,5]

  describe "my zip" $
    it "zips or combines two list together" $ do
      zip' ['x','y','z'] ['a','b','c'] `shouldBe` [('x','a'),('y','b'),('z','c')]
      zip' ['a'] ([] :: String) `shouldBe` []

  describe "segregate" $
    it "Seperates evens into the 1st list and odds into the second." $
      segregate ([1,2,3,4,5,6,7] :: [Int]) `shouldBe` [[2,4,6],[1,3,5,7]]

  describe "isMember" $
    it "returns true if the given element is a member of the given list." $ do
      isMember 'a' "how are you doing" `shouldBe` True
      isMember 'a' ['a','b'] `shouldBe` True
      isMember 'a' ['c','d'] `shouldBe` False

  describe "isSorted" $ do
    it "returns true if the list is sorted in increasing order" $ do
      isSorted ['a','b','c'] `shouldBe` True
      isSorted ([1,2,3,4] :: [Int]) `shouldBe` True
      isSorted ([1,2] :: [Int]) `shouldBe` True
      isSorted ([1] :: [Int]) `shouldBe` True
      isSorted ([] :: String) `shouldBe` True
    it "returns false if the list is not sorted in increasing order" $ do
      isSorted ['c','b','a'] `shouldBe` False
      isSorted ([4,3,2,1] :: [Int]) `shouldBe` False
      isSorted ([2,1] :: [Int]) `shouldBe` False
