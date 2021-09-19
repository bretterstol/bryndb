module Index.IndexTypes where
--https://stackoverflow.com/questions/20309501/how-to-implement-b-tree-in-haskell
import Data.Vector

type Keys k = Vector k

type Values v = Vector v

data Leaf k v = Leaf (Keys k) (Values v) (Maybe (Leaf k v))

type Children k v = Vector (BTree k v)

data Node k v = Node (Keys k) (Children k v)

data BTree k v = BTreeNode (Node k v) | BTreeLeaf (Leaf k v)

newtype BTreeRoot k v = BTreeRoot (BTree k v)


