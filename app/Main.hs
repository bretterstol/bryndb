module Main where

import ParserToken

main :: IO ()
main = do
  file  <- readFile "test.bdb"
  print $ testParse file

