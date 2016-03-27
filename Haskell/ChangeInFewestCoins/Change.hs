module Change where

-- A greedy algorithm that changes n cents (as input) into the fewest coins
-- by returning a string where q = quarter, d = dime, n = nickel, and p = penny
change :: Int -> String
change cents = go cents [] where
  go 0 acc = acc
  go n acc
    | n `div` 25 > 0 = go (n `mod` 25) (replicate (n `div` 25) 'q') ++ acc
    | n `div` 10 > 0 = go (n `mod` 10) (replicate (n `div` 10) 'd') ++ acc
    | n `div` 5  > 0 = go (n `mod` 5)  (replicate (n `div` 5)  'n') ++ acc
    | otherwise      = go (n `mod` 1)  (replicate (n `div` 1)  'p') ++ acc

