module Lychrel where

limit :: Integer
limit = 1000

getLychrelInteration :: Integer -> Integer
getLychrelInteration n = converge n 0

converge :: Integer -> Integer -> Integer
converge n interation = if interation >= limit || isPalindrome n
                           then interation
                           else converge (n + reverseInt n) (interation + 1)

isPalindrome :: Integer -> Bool
isPalindrome n = reverseInt n == n

reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

