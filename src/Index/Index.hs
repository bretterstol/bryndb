module Index.Index where

import Data.Vector
import Index.IndexTypes



keys :: [k] -> Keys k
keys = fromList

values :: [v] -> Values v
values = fromList

leaves :: [Leaf k v] -> Children k v
leaves = fromList . fmap BTreeLeaf

leaf :: Keys k -> Values v -> Maybe (Leaf k v) -> Leaf k v
leaf = Leaf

node :: Keys k -> Children k v -> BTree k v
node ks = BTreeNode . Node ks


createTree :: k -> v -> BTree k v
createTree k v =  node (keys [k]) (leaves [firstLeaf]) where firstLeaf = leaf (keys [k]) (values [v]) Nothing

insert :: BTree -> k -> v -> BTree k v
insert tree k v = let
  newLeaf = leaf (key ++ k) (val ++v) Nothing
  in node (key ++ k) (leaves [newLeaf])

indextest = let
  root  = node (keys [3, 5]) (leaves [leaf1, leaf2, leaf3])
  leaf1 = leaf (keys [1, 2]) (values ["d1", "d2"]) (Just leaf2)
  leaf2 = leaf (keys [3, 4]) (values ["d3", "d4"]) (Just leaf3)
  leaf3 = leaf (keys [5, 6, 7]) (values ["d5", "d6", "d7"]) Nothing
  in root