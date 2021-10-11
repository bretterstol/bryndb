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
  let next = BTree.BTree.insert 5 "sekjfsd" newTree
  print next
  let a =  BTree.BTree.insert 7 "sed" next
  let a1 =  BTree.BTree.insert 6 "sd" a
  print a1
  let a2 =  BTree.BTree.insert 8 "s" a1
  let a3 = BTree.BTree.insert 10 "fsd" a2
  print a3
  let a4 = BTree.BTree.insert 11 "fsd" a3
  print a4
  let a5 = BTree.BTree.insert 12 "yht" a4
  print a5
  let a6 = BTree.BTree.insert 13 "feweer" a5
  print a6
 -- let a7 = BTree.BTree.insert 14 "fqweqwe" a6
 -- let a8 = BTree.BTree.insert 15 "ytreyrey" a7

  --Aprint a8
