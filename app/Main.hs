module Main where

import FileSearch
import BdbValues
import FileInsert
import BTree.BTree 

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

main :: IO ()
main = do
  print $ BTree.BTree.insert 1 "hei" BTree.BTree.Nil
