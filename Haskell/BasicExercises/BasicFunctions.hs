module BasicFunctions where

import Data.List

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldl' (flip (:)) [] xs

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs
map' _ [] = []

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _ = []

-- Segregates evens into the 1st list and odds into the second
segregate :: Integral t => [t] -> [[t]]
segregate [] = []
segregate xs = go xs [] [] where
  go [] evens odds = [reverse' evens, reverse' odds]
  go (y:ys) evens odds
    | isEven y = go ys (y:evens) odds
    | otherwise = go ys evens (y:odds)

isEven :: Integral a => a -> Bool
isEven n = mod n 2 == 0

isMember :: Eq a => a -> [a] -> Bool
isMember _ [] = False
isMember x (y:ys)
  | x == y = True
  | otherwise = isMember x ys

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:ys) | x <= y = isSorted ys
                  | otherwise = False
isSorted _ = True

getElemAtIndex :: [a] -> Int -> Maybe a
getElemAtIndex [] _ = Nothing
getElemAtIndex (x:xs) i
  | i == 0 = Just x
  | otherwise = getElemAtIndex xs (i - 1)
