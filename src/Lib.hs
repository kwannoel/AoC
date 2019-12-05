module Lib where

import System.IO (readFile)

readFileNumber :: Int -> IO String
readFileNumber n = readFile $ "./input/" ++ (show n)

separateBy :: String -> Char -> [String]
separateBy str delimiter = go str delimiter []
  where go [] _ stringSegment = [stringSegment]
        go (x:xs) d stringSegment | x == delimiter = stringSegment : go xs delimiter []
                                  | otherwise = go xs delimiter (stringSegment ++ [x])  
