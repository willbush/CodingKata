module Golf where

import Data.List (genericDrop, foldl')
import qualified Data.Vector.Unboxed as V

-- | builds a list of skipBy lists for the given list where the nth number
-- passed to skipBy ranges from 0 to the length list - 1.
skips :: [a] -> [[a]]
skips [] = []
skips xs = fmap (`skipBy` xs) [0..(length xs - 1)]

-- | skips by n elements, takes the head, and repeats until the list is empty.
-- In other words, it drops n elements, appends the head to the result, and
-- repeats until the list is empty.
skipBy :: (Integral t) => t -> [a] -> [a]
skipBy n list = reverse $ go (genericDrop n list) []
  where go (x:xs) result = go (genericDrop n xs) (x:result)
        go [] result     = result

-- | Returns a list of local maxima from the given list.
-- Local maxima here is defined to be a number y that is strictly larger than
-- the number before it x and the number after it z. Only numbers that have a
-- a number before and after it can be local maxima.
localMaxima :: [Integer] -> [Integer]
localMaxima list = reverse $ go list []
  where go xs@(x:y:z:_) result = if y > x && y > z
                                 then go (drop 1 xs) (y:result)
                                 else go (drop 1 xs) result
        go _ result            = result

-- | creates a histogram string from a given int list.
-- The histogram chart is expressed by asterisks in a column above the number.
-- One asterisk for each occurrence in the list.
histogram :: [Int] -> String
histogram list = go (buildFrequencies list) "==========\n0123456789\n" 
  where go freqs chart  =
          let chartFreqs = chartFrequencies freqs
          in case chartFreqs of
            "          " -> chart
            _            -> go (V.map (\n -> n - 1) freqs) (chartFreqs ++ "\n" ++ chart)

-- | builds a vector of frequencies where the value at an index is the number
-- of occurrences for a number in the given int list. The number in the given
-- int list is used as the index into the vector array.
buildFrequencies :: [Int] -> V.Vector Int
buildFrequencies = foldl' f (V.replicate 10 0)
  where f acc x = acc V.// [(x, acc V.! x + 1)]

-- | Takes a vector of frequencies and builds a string where the character at
-- an index is either a space (frequency is 0) or an asterisks (frequency > 0)
chartFrequencies :: V.Vector Int -> String
chartFrequencies = V.foldr (\x acc -> if x > 0 then '*':acc else ' ':acc) ""
