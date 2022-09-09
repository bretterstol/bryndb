module BTree.Index where


import Control.Concurrent.STM
import qualified Data.Map as M
import BTree.BTreeTypes (BTree)
import qualified BTree.BTree as BT
import BdbValues

type IndexKeyValue a b = M.Map String (BTree a b)

createIndexHandler :: STM (TVar (IndexKeyValue a b))
createIndexHandler = newTVar M.empty

insertIndex :: String -> BTree a b -> TVar (IndexKeyValue a b) -> STM ()
insertIndex key index var = modifyTVar var $ M.insert key index

getIndex :: String -> TVar (IndexKeyValue a b) -> STM (Maybe (BTree a b))
getIndex key var = do
  hM <- readTVar var
  return $ M.lookup key hM


insertBMap :: String -> BValue -> BTree BValue String -> TVar (IndexKeyValue BValue String) -> STM ()
insertBMap name (BMap liste) tree index = mapM_ (\(key, val) ->
  case val of 
    b@(BMap _) -> insertBMap name b tree index
    _ -> insertValue name key val tree index
  ) liste
insertBMap _ _ _ _ = return ()

insertValue :: String -> String -> BValue -> BTree BValue String -> TVar (IndexKeyValue BValue String) -> STM ()
insertValue fileName name value tree index = do
  maybeExistingIndex <- getIndex name index
  case maybeExistingIndex of
    Just existingIndex -> insertIndex name (BT.insert value fileName existingIndex) index
    Nothing -> insertIndex name (BT.createTree value fileName) index