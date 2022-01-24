module Main where

import BdbValues
import qualified BTree.BTree as B
import BTree.BTreeTypes
import Control.Concurrent.STM
import Index
import qualified File.FileInsert as File

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

tree :: BTree Int String
tree = B.insert 3 "seg" (B.createTree 2 "hei")
main :: IO ()
main = do
  indexDict <- atomically createIndexHandler
  fileIndex <- File.insert test
  print fileIndex
  
  

