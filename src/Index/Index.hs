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