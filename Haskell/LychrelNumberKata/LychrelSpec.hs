import Test.Hspec
import Lychrel

main :: IO ()
main = hspec $ do
    describe "Lychrel Numbers" $ do
        context "when given known non-Lychrel numbers" $ do
            it "converges 1 to a palindrome in 0 iterations" $
                getLychrelInteration 1 `shouldBe` 0

            it "converges 9 to a palindrome in 0 iterations" $
                getLychrelInteration 9 `shouldBe` 0

            it "converges 10 to a palindrome in 1 iterations" $
                getLychrelInteration 10 `shouldBe` 1

            it "converges 11 to a palindrome in 0 iterations" $
                getLychrelInteration 11 `shouldBe` 0

            it "converges 12 to a palindrome in 1 iterations" $
                getLychrelInteration 12 `shouldBe` 1

            it "converges 19 to a palindrome in 2 iterations" $
                getLychrelInteration 19 `shouldBe` 2

            it "converges 78 to a palindrome in 4 iterations" $
                getLychrelInteration 78 `shouldBe` 4

            it "converges 89 to a palindrome in 24 iterations" $
                getLychrelInteration 89 `shouldBe` 24

            it "converges 1186060307891929990 to a palindrome in 261 iterations" $
                getLychrelInteration 1186060307891929990 `shouldBe` 261

        context "when given possible Lychrel numbers" $
            it "does not converge 196, but instead hits the limit" $
                getLychrelInteration 196 `shouldBe` 1000

    describe "isPalindrome" $
        context "when given a integer n" $ do
            it "returns true when given 1" $
                isPalindrome 1 `shouldBe` True

            it "returns true when given 9" $
                isPalindrome 9 `shouldBe` True

            it "returns false when given 10" $
                isPalindrome 10 `shouldBe` False

            it "returns false when given 21" $
                isPalindrome 21 `shouldBe` False

            it "returns true when given 121" $
                isPalindrome 121 `shouldBe` True

            it "returns true when given 123454321" $
                isPalindrome 123454321 `shouldBe` True
