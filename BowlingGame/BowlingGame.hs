-- My haskell implementation of Uncle Bob's Bowling Game kata.
-- It just consist of a function that scores a bowling game.
-- it assumes the given list of rolls represent a valid game.

module BowlingGame where

scoreGame :: [Int] -> Int
scoreGame rolls = go rolls 0 (0 :: Int) where
  go _ score 10 = score
  go (firstInFrame:x:xs) score frameCount
    | isStrike firstInFrame  = go (x:xs) (score + firstInFrame + x + head xs) (frameCount + 1)
    | isSpare firstInFrame x = go xs     (score + firstInFrame + x + head xs) (frameCount + 1)
    | otherwise              = go xs     (score + firstInFrame + x)           (frameCount + 1)
  go _ score _ = score

isSpare :: Int -> Int -> Bool
isSpare firstInFrame secondInFrame = firstInFrame + secondInFrame == 10

isStrike :: Int -> Bool
isStrike roll = roll == 10
