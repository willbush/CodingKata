module FizzBuzzKata where

fizzBuzzify :: (Show a, Integral a) => a -> String
fizzBuzzify x
  | isDivisibleBy3 x && isDivisibleBy5 x = "FizzBuzz"
  | isDivisibleBy3 x = "Fizz"
  | isDivisibleBy5 x = "Buzz"
  | otherwise = show x

isDivisibleBy3 :: Integral a => a -> Bool
isDivisibleBy3 x = x `mod` 3 == 0

isDivisibleBy5 :: Integral a => a -> Bool
isDivisibleBy5 x = x `mod` 5 == 0
