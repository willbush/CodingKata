module HW4 where

import Data.List

type Height = Integer

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
  deriving (Show, Eq)

fun1 :: [Integer] -> Integer
fun1 = foldl' (\acc x -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate f
  where f x = if even x then x `div` 2 else 3 * x + 1

createBalancedTree :: [a] -> Tree a
createBalancedTree = foldr insert' Leaf

insert' :: a -> Tree a -> Tree a
insert' x Leaf = Node 0 Leaf x Leaf
insert' x (Node _ lTree y rTree)
  | getHeight lTree <= getHeight rTree = let newLTree = insert' x lTree
                                         in Node (getHeight newLTree + 1) newLTree y rTree
  | otherwise                          = let newRTree = insert' x rTree
                                         in Node (getHeight newRTree + 1) lTree y newRTree

getHeight :: Tree a -> Height
getHeight (Node height _ _ _) = height
getHeight Leaf = -1

xor :: [Bool] -> Bool
xor = foldl' (\a b -> a && not b || not a && b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f base = foldr (flip f) base . reverse

-- | The Sieve of Sundaram method of finding primes up to a number N.
-- Note that the algorithm as described on Wikipedia cannot generate the prime
-- number 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let xs = [1..n]
                      filtered = xs \\ [i + j + 2 * i * j | i <- xs, j <- xs, i <= j]
                  in [2 * x + 1 | x <- filtered, 2 * x + 2 < n]

