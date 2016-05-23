import Test.Hspec

import Golf
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = hspec $ do

  describe "skipBy" $ do
    it "returns an empty list if given an empty list" $
      skipBy (0 :: Int) "" `shouldBe` []
    it "builds a list by repeatedly skipping by n elements and taking the head." $ do
      skipBy (0 :: Int) "hello!" `shouldBe` "hello!"
      skipBy (1 :: Int) "hello!" `shouldBe` "el!"
      skipBy (2 :: Int) "hello!" `shouldBe` "l!"
      skipBy (3 :: Int) "hello!" `shouldBe` "l"
      skipBy (4 :: Int) "hello!" `shouldBe` "o"
      skipBy (5 :: Int) "hello!" `shouldBe` "!"

  describe "skips" $ do
    it "returns an empty list if given an empty list" $
      skips "" `shouldBe` []
    it "builds a list of skipBy lists" $ do
      skips "A" `shouldBe` ["A"]
      skips [1] `shouldBe` [[1 :: Int]]
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips [1 :: Int, 2, 3, 4, 5] `shouldBe` [[1, 2, 3, 4, 5], [2, 4], [3], [4], [5]]

  describe "localMaxima" $ do
    it "returns an empty list if given an empty list" $
      localMaxima [] `shouldBe` []
    it "returns a list of local maxima" $ do
      localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
      localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
      localMaxima [1, 2, 3, 4, 5] `shouldBe` []

  describe "buildFrequencies" $ do
    it "returns an empty vector when given an empty list" $
      buildFrequencies [] `shouldBe` V.replicate 10 0
    it "returns a vector of frequencies when given a list of ints" $ do
      buildFrequencies [0,1,2,3,4,5,6,7,8,9] `shouldBe` V.replicate 10 1
      buildFrequencies [0,0,1,2,2,2,3,3,4,4] `shouldBe` V.fromList [2,1,3,2,2,0,0,0,0,0]
      buildFrequencies [0,1,1,1,2,2,2,3,3,4,4,9,9] `shouldBe` V.fromList [1,3,3,2,2,0,0,0,0,2]

  describe "chartFrequencies" $ do
    it "returns a string of 10 spaces when given a Vector of all 0 frequencies" $
      chartFrequencies (V.replicate 10 0) `shouldBe` replicate 10 ' '
    it "returns a string of 10 asterisk when given a Vector of all 1 frequencies" $
      chartFrequencies (V.replicate 10 1) `shouldBe` replicate 10 '*'
    it "returns a string of 10 spaces when given a Vector of all 1 frequencies" $
      chartFrequencies (V.fromList [1,3,3,2,2,0,0,0,0,2]) `shouldBe` "*****    *"

  describe "histogram" $ do
    it "returns an empty histogram chart when given an empty list" $
      histogram [] `shouldBe` "==========\n0123456789\n"
    it "can create a histogram chart from a simple list" $ do
      let given = [0,1,2,3,4,5,6,7,8,9]
      let exptected = "**********\n==========\n0123456789\n"
      histogram given `shouldBe` exptected
    it "can create a histogram chart from a complicated list" $ do
      let given = [0,0,0,2,2,3,4,4,5,5,5,6,7]
      let exptected = "*    *    \n* * **    \n* ******  \n==========\n0123456789\n"
      histogram given `shouldBe` exptected

