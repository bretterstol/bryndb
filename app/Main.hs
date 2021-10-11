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
  print a
  let a1 =  BTree.BTree.insert 6 "sd" a
  print a1
  let a2 =  BTree.BTree.insert 16 "s" a1
  print a2
  let a3 = BTree.BTree.insert 10 "fsd" a2
  print a3
  print $ Node [2::Int] [Leaf [Value (1, [2::Int])]] 1 == Node [2] [Leaf [Value (1, [2])]] 1
  let a4 = BTree.BTree.insert 11 "fsd" a3
  print a4
  let a5 = BTree.BTree.insert 12 "yht" a4
  --print a5
  let a6 = BTree.BTree.insert 13 "feweer" a5
  --print a6
  let a7 = BTree.BTree.insert 14 "fqweqwe" a6
  --print a7
  let a8 = BTree.BTree.insert 15 "ytreyrey" a7
  --print a8
  let a9 = BTree.BTree.insert 22 "yht" a8
  --print a9
  let a10 = BTree.BTree.insert (-1) "feweer" a9
  --print a10
  let a11 = BTree.BTree.insert 31 "fqweqwe" a10
  --print a11
  let a12 = BTree.BTree.insert 43 "ytreyrey" a11
  --print a12
  let a13 =BTree.BTree.insert (-13) "ytreyrey" a12 
  print a13
