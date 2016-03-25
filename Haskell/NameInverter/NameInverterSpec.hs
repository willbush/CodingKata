module NameInverterSpec where

import Test.Hspec
import NameInverter

main :: IO ()
main = hspec $ do
    describe "invertName" $ do
        it "returns empty string when given null" $
            invertName [] `shouldBe` ""

        it "returns empty string when given an empty string" $
            invertName "" `shouldBe` ""

        it "returns 'Name' when given 'Name'" $
            invertName "Name" `shouldBe` "Name"

        it "returns 'Last, First' when given 'First Last'" $
            invertName "First Last" `shouldBe` "Last, First"

        it "returns 'Last, First' when given ' First  Last '" $
            invertName " First  Last  " `shouldBe` "Last, First"

        it "can remove Mr. honorifics" $
            invertName "Mr. First  Last " `shouldBe` "Last, First"

        it "can remove Mrs. honorifics" $
            invertName "Mrs. First  Last " `shouldBe` "Last, First"

        it "can remove Dr. honorifics" $
            invertName "Dr. First  Last " `shouldBe` "Last, First"

        it "keeps post nominals at the end" $
            invertName "First Last Sr." `shouldBe` "Last, First Sr."

        it "keeps more post nominals at the end" $
            invertName "First Last Bs. Phd." `shouldBe` "Last, First Bs. Phd."
        it "should just work when given badly formatted input" $
            invertName " Dr.  John  Doe  III  esq.  " `shouldBe` "Doe, John III esq."
