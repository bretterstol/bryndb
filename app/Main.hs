module Main where

import BdbValues
import qualified BTree.BTree as B
import BTree.BTreeTypes
import Control.Concurrent.STM

test :: BValue
test = BMap [("ny", BString "hie"),("detteERKult", BNumber 1),("nes", BMap [("enda en", BString "hei på feg")])]

tree :: BTree Int String
tree = B.insert 3 "seg" (B.createTree 2 "hei")


indexStore :: IO (TVar [(String, BTree Int String)])
indexStore = newTVarIO []

updateIndexStore ::  String -> BTree Int String -> [(String, BTree Int String)]-> [(String, BTree Int String)]
updateIndexStore name test store = (name, test) : store

creatingADummyIndex :: BTree Int String
creatingADummyIndex = B.insert 4 "halla" $
   B.insert 5 "sekjfsd" $
   B.insert 7 "sed" $
   B.insert 6 "sd" $
   B.insert 16 "s" $
   B.insert 10 "fsd" $
   B.insert 11 "fsd" $
   B.insert 12 "yht" $
   B.insert 13 "feweer" $
   B.insert 14 "fqweqwe" $
   B.insert 15 "ytreyrey" $
   B.insert 22 "yht" $
   B.insert (-1) "feweer" $
  B.insert 31 "fqweqwe"  $
  B.insert 43 "ytreyrey" $
  B.insert 54 "ytreyrey" $
  B.insert 30 "ytreyrey" $
  B.insert 17 "ytreyrey" $
  B.insert 14 "ytreyrey" tree

main :: IO ()
main = do
  let dummy = creatingADummyIndex
  myStore <- indexStore
  atomically $ modifyTVar' myStore $ updateIndexStore "test" dummy
  indexes <- readTVarIO myStore
  mapM_ (\(name, tree) -> do
    print name
    print $ B.find 14 tree
    ) indexes
