module Main where

import FileSearch
import BdbValues
import FileInsert
import Index.Index

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

main :: IO ()
main = do
  res <- searchAsync ("detteERKult", BNumber 1.0)
  print res
  print $ createTree 1 "hei"