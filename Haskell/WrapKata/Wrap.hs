module Wrap where

import Data.Char
import Data.List
import Data.Maybe

wrap :: String -> Int -> String
wrap [] _ = []
wrap xs width = if length xs <= width then xs else
  let breakPoint = fromMaybe width (lastIndexOf ' ' $ take width xs)
  in take breakPoint xs ++ "\n" ++ wrap (dropWhile isSpace $ drop breakPoint xs) width

lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf element xs = let indices = elemIndices element xs
                         in case indices of
                              [] -> Nothing
                              _ -> Just $ last indices

