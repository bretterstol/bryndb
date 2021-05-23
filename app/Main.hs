module Main where

import ParseFile
import BdbValues

main :: IO ()
main = do
  file  <- readFile "test2.bdb"
  print $ parseBDB file

