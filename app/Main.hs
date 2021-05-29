module Main where

import FileSearch
import BdbValues
import FileInsert

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

main :: IO ()
main = do
  res <- search ("_id", BString "9156721d-c0e0-4e2c-b934-c1ac88f26f7d")
  print res
