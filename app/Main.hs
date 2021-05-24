module Main where

import ParserFull

main :: IO ()
main = do
  file  <- readFile "test2.bdb"
  print $ testParse file

