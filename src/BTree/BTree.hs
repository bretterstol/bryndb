module BTree.BTree where
import Data.List
import BTree.BTreeTypes
import Utils


insert :: (Ord k) => k -> v -> BTree k v -> BTree k v
insert key value Nil = Node [key] [leaf, Nil] where leaf = Leaf (key, [value])
insert key value tree = insertKeyVal key value tree


insertKeyVal ::(Ord k) => k -> v -> BTree k v -> BTree k v
insertKeyVal key val Nil = Node [key] [Leaf (key, [val])]
insertKeyVal key val leaf@(Leaf _) = Node [key] $ sort [leaf, newLeaf] where newLeaf = Leaf (key, [val])
insertKeyVal key val (Node keys children@[Leaf _, _]) = Node keys [insertKeyVal key val (children !! getChildIndex key keys)]
insertKeyVal key val (Node keys children) = Node keys $ insertKeyVal key val (children !! getChildIndex key keys) : children


findLeaf ::(Ord k) => k -> BTree k v -> BTree k v
findLeaf _ Nil = Nil
findLeaf _ leaf@(Leaf _) = leaf
findLeaf key (Node keys children) = findLeaf key $ children !! getChildIndex key keys


getChildIndex ::(Ord k) => k -> [k] -> Int
getChildIndex key keys = case safeHead (getChildIndexes key keys) of
  Just a -> a
  Nothing -> length keys

getChildIndexes :: (Ord k) => k -> [k] -> [Int]
getChildIndexes key = findIndices (key <=)