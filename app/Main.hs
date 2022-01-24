module Main where

import BdbValues
import qualified BTree.BTree as B
import BTree.BTreeTypes
import Control.Concurrent.STM
import qualified Index as I
import qualified File.FileInsert as File

test :: BValue
test = BMap [("nyt", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")]), ("ny", BString "ENDA EN TEST")]


test2 :: BValue
test2 = BMap [("ny", BString "hieee"),("detteERKult", BNumber 202),("natta", BMap [("enda masse", BString "hei på feg")]), ("ny", BString "TEST ivei")]

tree :: BTree BValue String
tree = (B.createTree BNull "")
main :: IO ()
main = do
  indexDict <- atomically I.createIndexHandler
  fileIndex <- File.insert test
  fileIndex2 <- File.insert test2
  atomically $ I.insertBMap  fileIndex test tree indexDict 
  atomically $ I.insertBMap  fileIndex2 test2 tree indexDict 
  val <- atomically $ readTVar indexDict
  print val 
  
  

