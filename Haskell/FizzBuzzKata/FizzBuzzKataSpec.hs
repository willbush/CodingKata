module FizzBuzzKataSpec where

import Test.Hspec
import FizzBuzzKata

main :: IO ()
main = hspec $
  describe "fizzBuzzify" $ do
    it "returns 1 when given 1" $
      fizzBuzzify (1 :: Int) `shouldBe` "1"
    it "returns 2 when given 2" $
      fizzBuzzify (2 :: Int) `shouldBe` "2"
    it "returns Fizz when given 3" $
      fizzBuzzify (3 :: Int) `shouldBe` "Fizz"
    it "returns Buzz when given 5" $
      fizzBuzzify (5 :: Int) `shouldBe` "Buzz"
    it "returns FizzBuzz when given 15" $
      fizzBuzzify (15 :: Int) `shouldBe` "FizzBuzz"
