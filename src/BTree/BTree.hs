module BTree.BTree where

data BTree a b = Nil | Leaf (a, [b])  | Node  [(a, BTree a b)] deriving Show


insert :: k -> v -> BTree k v -> BTree k v
insert key value Nil = Node [(key, leaf)] where leaf = Leaf (key, [value])
insert _ _ _ = Nil