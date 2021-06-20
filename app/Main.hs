module Main where

import FileSearch
import BdbValues
import FileInsert

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

main :: IO ()
main = do
  res <- searchAsync ("detteERKult", BNumber 1.0)
  print res
