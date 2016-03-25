module WrapSpec where

import Test.Hspec
import Wrap

main :: IO ()
main = hspec $ do
  describe "wrap" $ do
    it "should return an empty string when given nothing" $ do
      let width = 1
      wrap [] width `shouldBe` []

    it "should return x when given x with a width of 1" $ do
      let width = 1
      wrap "x" width `shouldBe` "x"

    it "should return x\\nx when given xx with a width of 1" $ do
      let width = 1
      wrap "xx" width `shouldBe` "x\nx"
      wrap "xxx" width `shouldBe` "x\nx\nx"

    it "should return x\\nx when given x x with a width of 1" $ do
      let width = 1
      wrap "x x" width `shouldBe` "x\nx"

    it "should return x\\nxx when given x xx with a width of 3" $ do
      let width = 3
      wrap "x xx" width `shouldBe` "x\nxx"

    it "should be able to handle complex strings" $ do
      let width = 5
      wrap "The quick brown fox" width `shouldBe` "The\nquick\nbrown\nfox"

  describe "lastIndexOf" $
    it "should return the last index of the given element for a list" $ do
      lastIndexOf 'b' [] `shouldBe` Nothing
      lastIndexOf 'b' "ab" `shouldBe` Just 1
