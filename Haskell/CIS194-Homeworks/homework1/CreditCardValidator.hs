module CreditCardValidator where

import Data.List

isValidCreditCard :: Integer -> Bool
isValidCreditCard xs = n `mod` 10 == 0 where
  n = (sumDigits . doubleEveryOther . toDigits) xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = go (reverse' xs) [] where
  go []       acc = acc
  go (x:y:ys) acc = go ys (2 * y : x : acc)
  go (y:ys)   acc = go ys (y : acc)

toDigits :: Integer -> [Integer]
toDigits x = go x [] where
  go n acc | n <= 0          = []
           | isDivisibleBy10 n = go (n `div` 10) (n `mod` 10 : acc)
           | otherwise = n : acc

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldl' (flip (:)) [] xs

sumDigits :: [Integer] -> Integer
sumDigits xs = go xs 0 where
  go [] acc = acc
  go (y:ys) acc | isDivisibleBy10 y = go ys acc + sum (toDigits y)
                | otherwise = go ys (acc + y)

isDivisibleBy10 :: Integer -> Bool
isDivisibleBy10 n = n `div` 10 > 0
