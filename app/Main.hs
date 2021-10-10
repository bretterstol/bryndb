module Main where

import FileSearch
import BdbValues
import FileInsert
import BTree.BTree
import BTree.BTreeTypes

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

tree :: BTree Int String
tree = BTree.BTree.insert 3 "seg" (BTree.BTree.createTree 2 "hei")
main :: IO ()
main = do
  print tree
  let newTree = BTree.BTree.insert 4 "halla" tree
  print newTree
  print $ BTree.BTree.insert 5 "sekjfsd" newTree
