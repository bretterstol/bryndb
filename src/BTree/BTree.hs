module BTree.BTree (
    insert
  , find
  , createTree
) where


import qualified Data.List  as L
import BTree.BTreeTypes
import Utils

treeSize :: Int
treeSize = 2

createTree :: k -> v -> BTree k v
createTree key val = Leaf [Value (key, [val])]

insert :: (Ord k) => k -> v -> BTree k v -> BTree k v
insert key val (Leaf values) =
  let valueLength = length newValues
      newValues = addValue key val values
  in if valueLength <= (treeSize + 1) then Leaf newValues
    else let Value (medianValue, _) = getMedianValue newValues
         in Node [medianValue] [lowerLeaves medianValue newValues, higherLeaves medianValue newValues] 1
insert key val (Node keys children size) =
  let child = getChild key keys children
  in refactor keys (map (\c -> if c == child then insert key val child else c) children) size


find :: (Ord k) => k -> BTree k v -> Maybe (Value k v)
find key (Leaf values) = findKeyInValues key values
find key (Node keys children _) =
  let child = getChild key keys children
  in find key child

findKeyInValues :: (Ord k) => k -> [Value k v] -> Maybe (Value k v)
findKeyInValues key = L.find (\(Value (k, _)) -> key == k)

addValue :: (Ord k) => k -> v -> [Value k v] -> [Value k v]
addValue key value values = case matchedValue of
  Just matched -> addInnerValue value matched : rest
  Nothing -> Value (key, [value]) : rest
  where matchedValue = findKeyInValues key values
        rest = filter (\(Value (k, _)) -> k /= key) values

addInnerValue :: v -> Value k v -> Value k v
addInnerValue value (Value (key,innerValues)) = Value (key, value : innerValues)

getChild :: (Ord k) =>  k -> [k] -> [BTree k v] -> BTree k v
getChild key keys children = children !! index 
  where index = getChildIndex key keys

lowerLeaves :: (Ord a) => a -> [Value a b] -> BTree a b
lowerLeaves key values = Leaf $ filterValues (<= key) values

higherLeaves :: (Ord a) => a -> [Value a b] -> BTree a b
higherLeaves key values = Leaf $ filterValues (> key) values

filterValues :: (a -> Bool) -> [Value a b] -> [Value a b]
filterValues func =  filter (\(Value (a, _)) -> func a)

getMedianValue :: (Ord a) => [Value a b] -> Value a b
getMedianValue values
  | odd (length values) && (length values > 1) = L.sort values !!(getMedianIndex values + 1)
 | otherwise = L.sort values !! getMedianIndex values

getMedianIndex :: [a] -> Int
getMedianIndex = divideByTwo . length

getMedian :: (Ord a) => [a] -> a
getMedian keys = L.sort keys !! getMedianIndex keys

divideByTwo :: Int -> Int
divideByTwo val = val `div` 2

getChildIndex ::(Ord k) => k -> [k] -> Int
getChildIndex key keys = case safeHead (getChildIndexes key keys) of
  Just a -> a
  Nothing -> length keys


getChildIndexes :: (Ord k) => k -> [k] -> [Int]
getChildIndexes key = L.findIndices (key <=)

refactor :: (Ord a) => [a] -> [BTree a b] -> Int -> BTree a b
refactor keys children size
  | all isLeaf children = Node keys children size
  | not $ sameHeightChildren children = refactorUpwards $ refactorInner keys children
  | otherwise = refactorUpwards $ Node keys children size

sameHeightChildren :: [BTree a b] -> Bool
sameHeightChildren children = all ((== nodeOr0 (head children)) . nodeOr0) children

nodeOr0 :: BTree a b -> Int
nodeOr0 tree = case tree of
  Node _ _ s -> s
  _ -> 0

refactorUpwards :: (Ord a) => BTree a b -> BTree a b
refactorUpwards leaf@(Leaf _) = leaf
refactorUpwards node@(Node keys values height)
  | length keys <= treeSize = node
  | otherwise =
    let median = getMedian keys
    in Node [median] [
    Node (getLowerKeys median keys) (filter filterEmpty $ map (getLower median) values) height,
    Node (getHigherKeys median keys) (filter filterEmpty $ map (getHigher median) values) height
    ] (height + 1)


getLowerKeys :: (Ord a) => a -> [a] -> [a]
getLowerKeys median = filter (< median)

getHigherKeys :: (Ord a) => a -> [a] -> [a]
getHigherKeys median = filter (> median)

getLower :: (Ord a) => a -> BTree a b -> BTree a b
getLower key tree = case tree of
  Node a b s -> Node (getLowerKeys key a) (map (getLower key) b) s
  Leaf a -> Leaf (filterValues (<= key) a)

getHigher :: (Ord a) => a -> BTree a b -> BTree a b
getHigher key tree = case tree of
  Node a b s -> Node (getHigherKeys key a) (map (getHigher key) b) s
  Leaf a -> Leaf (filterValues (> key) a)

filterEmpty :: BTree a b -> Bool
filterEmpty tree = case tree of
  Leaf a -> not (null a)
  Node a _ _ -> not (null a)

refactorInner ::(Ord a) => [a] -> [BTree a b] -> BTree a b
refactorInner keys children =
  let  node@(Node innK innC size) = getBiggestTree children
       leaves = filter (/= node) children
  in Node (L.sort $ keys ++ innK) (L.sort $ innC ++ leaves) size

getInnerNode :: [BTree a b] -> BTree a b
getInnerNode children = head $ getInnerNodes children


getBiggestTree :: [BTree a b] -> BTree a b
getBiggestTree tree =
  let maxHeight = highest 0 tree
  in case L.find (treeHasSize maxHeight) tree of
    Just t -> t
    Nothing -> getInnerNode tree

treeHasSize :: Int -> BTree a b -> Bool
treeHasSize s tree = case tree of
  Leaf _ -> False
  Node _ _ hs -> s == hs

highest :: Int -> [BTree a b] -> Int
highest m tree
  | null tree = m
  | otherwise = highest (highestHeight m (head tree)) (tail tree)

highestHeight :: Int -> BTree a b -> Int
highestHeight maxSoFar tree = case tree of
  Leaf _ -> maxSoFar
  Node _ _ height -> if height > maxSoFar then  height else maxSoFar

getInnerNodes :: [BTree a b] -> [BTree a b]
getInnerNodes = filter isNode

isNode :: BTree a b -> Bool
isNode tree = case tree of
  Node {} -> True
  _ -> False

isLeaf :: BTree a b -> Bool
isLeaf tree = case tree of
  Leaf _ -> True
  _ -> False
  