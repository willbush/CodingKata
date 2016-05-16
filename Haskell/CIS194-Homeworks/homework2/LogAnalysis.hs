{-# OPTIONS_GHC -Wall #-}

-- | This module is my implementation of CIS 194 homework 2, which is just
-- something I'm doing for fun and to learn Haskell. Please see the README.md
-- file for more info.

module LogAnalysis where

import Data.List (foldl')
import Data.List.Split (splitOn)

type ErrorLevel = Int

data MessageType = Info
                 | Warning
                 | Error ErrorLevel
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | This takes the given number of log messages from the top of a log file.
takeFromLogFile :: Int -> FilePath -> IO [LogMessage]
takeFromLogFile numToTake filePath = take numToTake . parse <$> readFile filePath

-- | This takes a predicate that is used to filter the error messages by error level
-- and the file path to the error log. the returned IO list is sorted by time stamp
-- and contains only the filtered error message string.
filterLogFileByErrorLevelThat :: (ErrorLevel -> Bool) -> FilePath -> IO [String]
filterLogFileByErrorLevelThat predicate file
  = filterLogMsgsByErrorLevelThat predicate . parse <$> readFile file

-- | This takes a string which may contain multiple lines of log messages
-- and parses it into a list of log messages.
parse :: String -> [LogMessage]
parse xs = parseMessage <$> splitOn "\n" xs

-- | This takes a single log message string and parses into a LogMessage type.
parseMessage :: String -> LogMessage
parseMessage msg =
  case words msg of
    ("I":msgListTail) -> parseInfoMsg msgListTail
    ("W":msgListTail) -> parseWarningMsg msgListTail
    ("E":msgListTail) -> parseErrorMsg msgListTail
    _   -> Unknown msg

-- | This takes a list of "words" (i.e. what you get if you split a string on whitespace)
-- that all belong to a single log message of the type Info and parses it into a Info type.
parseInfoMsg :: [String] -> LogMessage
parseInfoMsg (x:xs) = LogMessage Info timestamp infoMsg
  where timestamp = read x :: TimeStamp
        infoMsg   = unwords xs
parseInfoMsg msg = Unknown (unwords msg)

-- | This takes a list of "words" (i.e. what you get if you split a string on whitespace)
-- that all belong to a single log message of the type warning and parses it into a Warning type.
parseWarningMsg :: [String] -> LogMessage
parseWarningMsg (x:xs) = LogMessage Warning timestamp infoMsg
  where timestamp = read x :: TimeStamp
        infoMsg = unwords xs
parseWarningMsg msg = Unknown (unwords msg)

-- | This takes a list of "words" (i.e. what you get if you split a string on whitespace)
-- that all belong to a single log message of the type error and parses it into a Error type.
parseErrorMsg :: [String] -> LogMessage
parseErrorMsg (x:y:zs) = LogMessage (Error errorLevel) timestamp errorMsg
  where errorLevel = read x :: ErrorLevel
        timestamp  = read y :: TimeStamp
        errorMsg   = unwords zs
parseErrorMsg msg = Unknown (unwords msg)

-- | This takes a predicate that is used to filter the error messages by error level
-- and a LogMessage list. the returned list is sorted by time stamp
-- and contains only the filtered error message string.
filterLogMsgsByErrorLevelThat :: (ErrorLevel -> Bool) -> [LogMessage] -> [String]
filterLogMsgsByErrorLevelThat _ [] =  []
filterLogMsgsByErrorLevelThat predicate messages =
  let sortedMsgs = sortLogMessages (buildTree messages)
      importantMsgs = filter (isErrorWithLevel predicate) sortedMsgs
  in fmap extractLogMessageString importantMsgs

-- | This builds a unbalanced binary search tree. How balanced the tree is largely depends on
-- the first insertion and how close its time stamp is to the median of all time stamps.
-- The tree is used to sort the messages by time stamp. As pointed out by the homework
-- exercise, there are better ways to sort and doing it this way is mostly for practice with
-- recursive data structures.
buildTree :: [LogMessage] -> MessageTree
buildTree = foldl' (flip insert) Leaf

-- | This inserts a log message into the message tree. Message tree is a binary search
-- tree with left child time stamp < parent time stamp and right child >= parent time stamp.
insert :: LogMessage -> MessageTree -> MessageTree
insert message tree = case extractTimeStamp message of
  Just time  -> insert' message time tree
  Nothing    -> tree

-- | This helper function takes the extracted time stamp from the log message to needs to be
-- inserted and then extracts the time stamp for the current node in the tree, compares the
-- time stamps and makes the proper recursive call or insertion.
insert' :: LogMessage -> TimeStamp -> MessageTree -> MessageTree
insert' msg _ Leaf = Node Leaf msg Leaf
insert' msg time tree@(Node left treeMsg right) =
  case extractTimeStamp treeMsg of
    Just treeTime -> if time < treeTime
                     then Node (insert' msg time left) treeMsg right
                     else Node left treeMsg (insert' msg time right)
    Nothing       -> tree

-- | This extracts a Maybe TimeStamp from a LogMessage since a LogMessage type can also be
-- Unknown.
extractTimeStamp :: LogMessage -> Maybe TimeStamp
extractTimeStamp (LogMessage _ timestamp _) = Just timestamp
extractTimeStamp _ = Nothing

-- | This takes a message tree, which is an unbalanced binary search tree and performs
-- an in order traversal to build a sorted log message list. However, unlike normal in
-- order traversals, this function traverses the right children first instead of the left.
-- This has the effect of visiting the nodes in order from greatest time stamp to least.
-- Visiting nodes in this order works well because it's easier to cons elements from
-- greatest to least into an empty list to get the resulting list sorted from least to
-- greatest. Otherwise, the list would of needed to be reversed.
sortLogMessages :: MessageTree -> [LogMessage]
sortLogMessages messageTree = sort messageTree []
  where sort (Node left msg right) sortedMsgs = sort left $ msg : sort right sortedMsgs
        sort Leaf sortedMsgs = sortedMsgs

-- | This takes a predicate (such as (>= 50)) that is used against the error level of the
-- given log message. If the log message is not of the type 'Error', then it will return false.
isErrorWithLevel :: (ErrorLevel -> Bool) -> LogMessage -> Bool
isErrorWithLevel predicate (LogMessage (Error level) _ _) = predicate level
isErrorWithLevel _ _ = False

-- | This extracts the message string from a log message.
extractLogMessageString :: LogMessage -> String
extractLogMessageString (LogMessage _ _ messageString) = messageString
extractLogMessageString _ = []

