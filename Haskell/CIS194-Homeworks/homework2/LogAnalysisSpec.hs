module LogAnalysisSpec where

import Test.Hspec

import LogAnalysis

main :: IO ()
main = hspec $ do
  describe "parseMessage" $ do
    it "should parse info messages" $ do
      let message = "I 9 Back from lunch"
      let expectedResult = LogMessage Info 9 "Back from lunch"
      parseMessage message `shouldBe` expectedResult
    it "should parse info messages with extra whitespace" $ do
      let message = "  I   9   Back   from   lunch  "
      let expectedResult = LogMessage Info 9 "Back from lunch"
      parseMessage message `shouldBe` expectedResult

    it "should parse warning messages" $ do
      let message = "W 5 I'm too cool for school"
      let expectedResult = LogMessage Warning 5 "I'm too cool for school"
      parseMessage message `shouldBe` expectedResult
    it "should parse warning messages with extra whitespace" $ do
      let message = "  W   5   I'm   too   cool  for  school "
      let expectedResult = LogMessage Warning 5 "I'm too cool for school"
      parseMessage message `shouldBe` expectedResult

    it "should parse error messages" $ do
      let message = "E 2 562 help help"
      let expectedResult = LogMessage (Error 2) 562 "help help"
      parseMessage message `shouldBe` expectedResult
    it "should parse error messages with extra whitespace" $ do
      let message = " E   2  562  help   help  "
      let expectedResult = LogMessage (Error 2) 562 "help help"
      parseMessage message `shouldBe` expectedResult

    it "should return Unknown type for a message with incorrect format" $ do
      let message = "blah"
      let expectedResult = Unknown "blah"
      parseMessage message `shouldBe` expectedResult

  describe "extractTimeStamp" $ do
    it "should extract a time stamp from an info message" $ do
      let message = LogMessage Info 10 "some message"
      extractTimeStamp message `shouldBe` Just 10
    it "should extract a time stamp from a warning message" $ do
      let message = LogMessage Warning 11 "some message"
      extractTimeStamp message `shouldBe` Just 11
    it "should extract a time stamp from an error message" $ do
      let message = LogMessage (Error 100) 12 "some message"
      extractTimeStamp message `shouldBe` Just 12
    it "should extract nothing from a unknown message type" $ do
      let message = Unknown "some message"
      extractTimeStamp message `shouldBe` Nothing

  describe "insert" $ do
    it "should be able to insert into an empty tree" $ do
      let message = LogMessage Info 10 "some message"
      insert message Leaf `shouldBe` Node Leaf message Leaf
    it "should be able to insert 3 messages" $ do
      let message1 = LogMessage Info 10 "some message"
      let message2 = LogMessage (Error 1) 5 "some message"
      let message3 = LogMessage Warning 15 "some message"

      let expectedLeftChild = Node Leaf message2 Leaf
      let expectedRightChild = Node Leaf message3 Leaf

      let tree = insert message3 $ insert message2 $ insert message1 Leaf

      tree `shouldBe` Node expectedLeftChild message1 expectedRightChild

  describe "buildTree" $ do
    it "should return just a Leaf when given an empty list" $
      buildTree [] `shouldBe` Leaf

    it "should build a message tree from a list of log message using insert" $ do
      let message1 = LogMessage Info 10 "some message"
      let message2 = LogMessage (Error 1) 5 "some message"
      let message3 = LogMessage Warning 15 "some message"

      let expectedLeftChild = Node Leaf message2 Leaf
      let expectedRightChild = Node Leaf message3 Leaf

      let expectedTree = Node expectedLeftChild message1 expectedRightChild

      buildTree [message1, message2, message3] `shouldBe` expectedTree

  describe "sortLogMessages" $ do
    it "should return an empty list if given a empty tree" $
      sortLogMessages Leaf `shouldBe` []
    it "should do an in order tree traversal and return a sorted list of messages" $ do
      let m1 = LogMessage Info 10 "some message"
      let m2 = LogMessage (Error 1) 5 "some message"
      let m3 = LogMessage Warning 15 "some message"
      let m4 = LogMessage Warning 2 "some message"
      let m5 = LogMessage Warning 7 "some message"
      let m6 = LogMessage Warning 13 "some message"
      let m7 = LogMessage Warning 20 "some message"

      let givenList = [m1, m2, m3, m4, m5, m6, m7]

      let expectedList = [m4, m2, m5, m1, m6, m3, m7]

      sortLogMessages (buildTree givenList) `shouldBe` expectedList

  describe "filterLogMsgsByErrorLevelThat" $
    it "returns a filtered list list of error messages" $ do
      let m1 = LogMessage (Error 1) 1 "error 1"
      let m2 = LogMessage (Error 2) 2 "error 2"
      let m3 = LogMessage (Error 3) 3 "error 3"
      let m4 = LogMessage (Error 4) 4 "error 4"
      let m5 = LogMessage (Error 5) 5 "error 5"
      let m6 = LogMessage (Error 6) 6 "error 6"

      -- | list can contain any message type
      let unsortedList = [m2, m5, m3, m1, m4, m6]
      filterLogMsgsByErrorLevelThat (< 3) unsortedList `shouldBe` ["error 1", "error 2"]
