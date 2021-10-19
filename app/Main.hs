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
  let newTree = BTree.BTree.insert 4 "halla" tree
  let next = BTree.BTree.insert 5 "sekjfsd" newTree
  let a =  BTree.BTree.insert 7 "sed" next
  let a1 =  BTree.BTree.insert 6 "sd" a
  let a2 =  BTree.BTree.insert 16 "s" a1
  let a3 = BTree.BTree.insert 10 "fsd" a2
  let a4 = BTree.BTree.insert 11 "fsd" a3
  let a5 = BTree.BTree.insert 12 "yht" a4
  let a6 = BTree.BTree.insert 13 "feweer" a5
  let a7 = BTree.BTree.insert 14 "fqweqwe" a6
  let a8 = BTree.BTree.insert 15 "ytreyrey" a7
  let a9 = BTree.BTree.insert 22 "yht" a8
  let a10 = BTree.BTree.insert (-1) "feweer" a9
  let a11 = BTree.BTree.insert 31 "fqweqwe" a10
  let a12 = BTree.BTree.insert 43 "ytreyrey" a11
  let a13 =BTree.BTree.insert (54) "ytreyrey" a12
  let a14 =BTree.BTree.insert 30 "ytreyrey" a13
  let a15 =BTree.BTree.insert 17 "ytreyrey" a14
  let a16 = BTree.BTree.insert 14 "ytreyrey" a15
  print $ find 14 a16
