import HW4
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "fun1" $
    it "should pass the following acceptance tests" $ do
      fun1 [6..20] `shouldBe` 92897280
      fun1 [10..20] `shouldBe` 3870720

  describe "fun2" $
    it "should pass the following acceptance tests" $ do
      map fun2 [1..10] `shouldBe` [0,2,40,6,30,46,234,14,276,40]
      map fun2 [10..20] `shouldBe` [40,212,58,100,248,562,30,178,294,424,60]

  describe "getHeight" $
    it "return tree height" $ do
      getHeight Leaf `shouldBe` -1
      getHeight (Node 1 Leaf 'A' Leaf) `shouldBe` 1
      getHeight (Node 2 Leaf 'A' Leaf) `shouldBe` 2

  describe "createBalancedTree" $ do
    it "return a leaf when given an empty list" $
      createBalancedTree "" `shouldBe` Leaf
    it "should build a balanced binary tree given a list of any type" $ do
      createBalancedTree ['A'] `shouldBe` Node 0 Leaf 'A' Leaf
      createBalancedTree "IJ" `shouldBe` Node 1 (Node 0 Leaf 'I' Leaf) 'J' Leaf
      let expectedTree =
            Node 3
              (Node 2
                (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
                'I'
                (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
              'J'
              (Node 2
                (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
                'H'
                (Node 0 Leaf 'C' Leaf))
      createBalancedTree "ABCDEFGHIJ" `shouldBe` expectedTree

  describe "xor" $
    it "should return true only if there are a odd number of True values" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $
    it "should behave just like the normal map function" $ do
      map' (+ 1) ([1..10] :: [Integer]) `shouldBe` map (+ 1) [1..10]
      map' (+ 2) ([1..10] :: [Integer]) `shouldBe` map (+ 2) [1..10]

  describe "myFoldl" $
    it "should behave just like the normal foldl function" $ do
      myFoldl (+) 100 ([1..10] :: [Integer]) `shouldBe` foldl (+) 100 [1..10]
      myFoldl (-) 100 ([1..10] :: [Integer]) `shouldBe` foldl (-) 100 [1..10]

  describe "sieveSundaram" $
    it "is a method for generating primes up to N" $
      sieveSundaram 100 `shouldBe` [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]







