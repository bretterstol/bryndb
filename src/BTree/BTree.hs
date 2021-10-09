module BTree.BTree where
import Data.List
import BTree.BTreeTypes
import Utils

insert :: (Ord k, Ord v) => k -> v -> BTree k v -> BTree k v
insert key value Nil = Node [key] [leaf] where leaf = Leaf (key, [value])
insert key value (Node keys values) = Node newKeys leafs
  where leafs = sortLeafs $ leaf : values
        leaf = Leaf (key, [value])
        newKeys = sort $ key : keys
insert _ _ _ = Nil

sortLeafs :: (Ord k) => [BTree k v] -> [BTree k v]
sortLeafs = sort


findLeaf ::(Ord k) => k -> BTree k v -> BTree k v
findLeaf _ Nil = Nil
findLeaf _ leaf@(Leaf _) = leaf
findLeaf key (Node keys children) = findLeaf key $ children !! getChildIndex key keys


getChildIndex ::(Ord k) => k -> [k] -> Int
getChildIndex key keys = case safeHead (getChildIndexs key keys) of
  Just a -> a
  Nothing -> length keys

getChildIndexs :: (Ord k) => k -> [k] -> [Int]
getChildIndexs key = findIndices (key <=)