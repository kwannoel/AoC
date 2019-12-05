module Main where

import Lib
import Solutions

main :: IO ()
main = do
  contents1 <- readFileNumber 1
  case parse1 contents1 of
    Just masses -> print $ solve1 masses
    Nothing -> print "parse error"
  contents2 <- readFileNumber 2
  case parse2 contents2 of
    Just intcode -> print . solve2b $ intcode 
    Nothing -> print "parse error"
