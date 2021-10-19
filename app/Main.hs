module Main where

import FileSearch
import BdbValues
import FileInsert
import qualified BTree.BTree as B
import BTree.BTreeTypes

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

tree :: BTree Int String
tree = B.insert 3 "seg" (B.createTree 2 "hei")
main :: IO ()
main = do
  let newTree = B.insert 4 "halla" tree
  let next = B.insert 5 "sekjfsd" newTree
  let a =  B.insert 7 "sed" next
  let a1 =  B.insert 6 "sd" a
  let a2 =  B.insert 16 "s" a1
  let a3 = B.insert 10 "fsd" a2
  let a4 = B.insert 11 "fsd" a3
  let a5 = B.insert 12 "yht" a4
  let a6 = B.insert 13 "feweer" a5
  let a7 = B.insert 14 "fqweqwe" a6
  let a8 = B.insert 15 "ytreyrey" a7
  let a9 = B.insert 22 "yht" a8
  let a10 = B.insert (-1) "feweer" a9
  let a11 = B.insert 31 "fqweqwe" a10
  let a12 = B.insert 43 "ytreyrey" a11
  let a13 =B.insert 54 "ytreyrey" a12
  let a14 =B.insert 30 "ytreyrey" a13
  let a15 =B.insert 17 "ytreyrey" a14
  let a16 = B.insert 14 "ytreyrey" a15
  print $ B.find 14 a16
