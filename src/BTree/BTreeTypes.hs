module BTree.BTreeTypes where


newtype Value a b = Value (a, [b]) deriving Show

data BTree a b = Leaf [Value a b] | Node [a] [BTree a b] deriving (Show)

-- BTreeRoot Size Height BTree
data BTreeRoot a b = BTreeRoot {
  size :: Int,
  height :: Int,
  btree :: BTree a b
}

instance (Eq a) => Eq (Value a b) where
  (Value (a1, _)) == (Value (a2, _)) = a1 == a2


instance (Ord a) => Ord (Value a b) where
  compare (Value (a1, _)) (Value (a2, _)) = compare a1 a2

instance (Eq a) => Eq (BTree a b) where
  (Leaf a1) == (Leaf a2) = a1 == a2
  (Node a1 _) == (Node a2 _) = a1 == a2
  Leaf _ == Node _ _ = False
  Node _ _ == Leaf _ = False

instance (Ord a) => Ord (BTree a b) where
  (Leaf a1) `compare` (Leaf a2) = a1 `compare` a2
  (Node a1 _) `compare` (Node a2 _) = a1 `compare` a2
  _ `compare` _ = EQ
