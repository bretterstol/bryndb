module Main where

import FileSearch
import BdbValues
import FileInsert
import BTree.BTree
import BTree.BTreeTypes

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

tree :: BTree Int String
tree = BTree.BTree.insert 2 "seg" (BTree.BTree.insert 1 "hei" Nil)
main :: IO ()
main = print $ findLeaf 1 tree
