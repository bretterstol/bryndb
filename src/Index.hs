module Index where


import Control.Concurrent.STM
import qualified Data.Map as M
import BTree.BTreeTypes (BTree)

type IndexKeyValue a b = M.Map String (BTree a b) 

createIndexHandler :: STM (TVar (IndexKeyValue a b))
createIndexHandler = newTVar M.empty

insertIndex :: String -> BTree a b -> TVar (IndexKeyValue a b) -> STM ()
insertIndex key index var = modifyTVar var $ M.insert key index

getIndex :: String -> TVar (IndexKeyValue a b) -> STM (Maybe (BTree a b))
getIndex key var = do
  hM <- readTVar var
  return $ M.lookup key hM
