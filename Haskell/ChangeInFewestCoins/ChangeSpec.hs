module ChangeSpec where

import Test.Hspec
import Change

main :: IO ()
main = hspec $
  describe "change" $
    it "should give a string of the fewest coins as change for n cents" $ do
    -- where q = quarter, d = dime, n = nickel, and p = penny
      change 0  `shouldBe` ("" :: String)
      change 25 `shouldBe` ("q" :: String)
      change 50 `shouldBe` ("qq" :: String)
      change 51 `shouldBe` ("pqq" :: String)
      change 67 `shouldBe` ("ppndqq" :: String)
