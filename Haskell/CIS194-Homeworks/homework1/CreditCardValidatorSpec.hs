module CreditCardValidatorSpec where

import Test.Hspec
import CreditCardValidator

main :: IO ()
main = hspec $
  describe "Validating Credit Card Numbers" $ do
    describe "toDigits" $
      it "converts an Integer to a list of digits for that number" $ do
        toDigits (-17) `shouldBe` []
        toDigits 0 `shouldBe` []
        toDigits 12`shouldBe` [1,2]
        toDigits 1234 `shouldBe` [1,2,3,4]

    describe "doubleEveryOther" $
      it "returns a list with every other element doubled from the right" $ do
        doubleEveryOther [1] `shouldBe` [1]
        doubleEveryOther [1,1] `shouldBe` [2,1]
        doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
        doubleEveryOther [1,1,1,1] `shouldBe` [2,1,2,1]

    describe "sumDigits" $
      it "sums each digit in the list (e.g. [1, 12] becomes 1 + 1 + 2 = 4)" $ do
        sumDigits [1] `shouldBe` 1
        sumDigits [1,2] `shouldBe` 3
        sumDigits [10,2,30,7] `shouldBe` 13
        sumDigits [16,7,12,5] `shouldBe` 22

    describe "isValidCreditCard" $
      it "returns True if the CC is valid" $ do
        isValidCreditCard 4012888888881881 `shouldBe` True
        isValidCreditCard 4012888888881882 `shouldBe` False
