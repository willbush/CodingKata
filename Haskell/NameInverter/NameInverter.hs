module NameInverter where

import Data.Char
import Data.List
import Text.Regex.Posix

invertName :: String -> String
invertName [] = ""
invertName name =
    let names = split (== ' ') name
     in formatName $ dropWhile isHonorific names

split :: (Char -> Bool) -> String -> [String]
split f s = case dropWhile f s of
                 "" -> []
                 s' -> w: split f s''
                    where (w, s'') = break f s'

isHonorific :: String -> Bool
isHonorific str = map toLower str =~ "(mr.|mrs.|dr.)" :: Bool

formatName :: [String] -> String
formatName [] = ""
formatName xs = let lastFirst = intercalate ", " $ reverse $ take 2 xs
                 in lastFirst ++ (formatPostNominals $ drop 2 xs)

formatPostNominals :: [String] -> String
formatPostNominals [] = ""
formatPostNominals xs = " " ++ unwords xs
