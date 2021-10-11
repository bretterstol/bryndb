module BTree.BTreeTypes where


newtype Value a b = Value (a, [b]) deriving Show

data BTree a b = Leaf [Value a b] | Node [a] [BTree a b]  Int deriving (Show)

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
  (Node a1 a2 a3) == (Node b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3
  Leaf _ == Node {}= False
  Node {} == Leaf _ = False

instance (Ord a) => Ord (BTree a b) where
  (Leaf a1) `compare` (Leaf a2) = a1 `compare` a2
  (Node a1 _ _) `compare` (Node a2 _ _) = a1 `compare` a2
  _ `compare` _ = EQ
