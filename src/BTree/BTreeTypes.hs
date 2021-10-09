module BTree.BTreeTypes where

data BTree a b = Nil | Leaf (a, [b]) | Node [a] [BTree a b] deriving (Show)

instance (Eq a) => Eq (BTree a b) where
  (Leaf (a, _)) == (Leaf (b, _)) = a == b
  _ == _ = False


instance (Ord a) => Ord (BTree a b) where
  compare (Leaf (a, _)) (Leaf (b, _)) = compare a b
  compare _ _ = EQ
