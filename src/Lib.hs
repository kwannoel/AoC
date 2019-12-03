module Lib where

import System.IO (readFile)

readFileNumber :: Int -> IO String
readFileNumber n = readFile $ "./input/" ++ (show n)
