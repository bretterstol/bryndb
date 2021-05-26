module Main where

import FileSearch
import BdbValues


main :: IO ()
main = do
  res <- search "Hei"
  mapM_ (print) res
