module Main where

import ParseFile

main :: IO ()
main = print =<< getFile "test.bdb"
