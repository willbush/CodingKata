-- My haskell implementation of Uncle Bob's Bowling Game kata.
-- It just consist of a function that scores a bowling game.
-- it assumes the given list of rolls represent a valid game.

module BowlingGame where

scoreGame :: [Int] -> Int
scoreGame xs = go xs 0 (0 :: Int) where
  go _ score 10 = score
  go rolls@(firstInFrame:x:y:_) score frameCount
    | isStrike firstInFrame  = go (drop 1 rolls)  (score + firstInFrame + x + y) (frameCount + 1)
    | isSpare firstInFrame x = go (drop 2 rolls)  (score + firstInFrame + x + y) (frameCount + 1)
    | otherwise              = go (drop 2 rolls)  (score + firstInFrame + x)     (frameCount + 1)
  go (firstInFrame:x:ys) score frameCount = go ys (score + firstInFrame + x)     (frameCount + 1)
  go _ score _ = score

isSpare :: Int -> Int -> Bool
isSpare firstInFrame secondInFrame = firstInFrame + secondInFrame == 10

isStrike :: Int -> Bool
isStrike roll = roll == 10
