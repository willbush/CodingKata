import Test.Hspec
import Lychrel

main :: IO ()
main = hspec $ do
    describe "Lychrel Numbers" $ do
        context "when given known non-Lychrel numbers" $ do
            it "converges 1 to a palindrome in 0 interations" $
                do getLychrelInteration 1 `shouldBe` 0

            it "converges 9 to a palindrome in 0 interations" $
                do getLychrelInteration 9 `shouldBe` 0

            it "converges 10 to a palindrome in 1 interations" $
                do getLychrelInteration 10 `shouldBe` 1

            it "converges 11 to a palindrome in 0 interations" $
                do getLychrelInteration 11 `shouldBe` 0

            it "converges 12 to a palindrome in 1 interations" $
                do getLychrelInteration 12 `shouldBe` 1

            it "converges 19 to a palindrome in 2 interations" $
                do getLychrelInteration 19 `shouldBe` 2

            it "converges 78 to a palindrome in 4 interations" $
                do getLychrelInteration 78 `shouldBe` 4

            it "converges 89 to a palindrome in 24 interations" $
                do getLychrelInteration 89 `shouldBe` 24

            it "converges 1186060307891929990 to a palindrome in 261 interations" $
                do getLychrelInteration 1186060307891929990 `shouldBe` 261

        context "when given possible Lychrel numbers" $ do
            it "does not converge 196, but instead hits the limit" $
                do getLychrelInteration 196 `shouldBe` 1000

    describe "isPalindrome" $ do
        context "when given a integer n" $ do
            it "returns true when given 1" $
                do isPalindrome 1 `shouldBe` True

            it "returns true when given 9" $
                do isPalindrome 9 `shouldBe` True

            it "returns false when given 10" $
                do isPalindrome 10 `shouldBe` False

            it "returns false when given 21" $
                do isPalindrome 21 `shouldBe` False

            it "returns true when given 121" $
                do isPalindrome 121 `shouldBe` True

            it "returns true when given 123454321" $
                do isPalindrome 123454321 `shouldBe` True
