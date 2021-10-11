module BTree.BTree where
import Data.List hiding (insert)
import BTree.BTreeTypes
import Utils

treeSize :: Int
treeSize = 2

createTree :: (Ord k) => k -> v -> BTree k v
createTree key val = Leaf [Value (key, [val])]

insert :: (Ord k) => k -> v -> BTree k v -> BTree k v
insert key val (Leaf values) =
  let vlenght = length values
      newValues = newValue : values
      newValue = Value (key, [val])
  in if vlenght < treeSize then Leaf $ sort newValues
    else let Value (medianValue, _) = getMedianValue newValues
         in Node [medianValue] [lowerLeafs medianValue newValues, higherLeafs medianValue newValues]
insert key val (Node keys children) =
  let index = getChildIndex key keys
      child = children !! index
  in refactor keys $ map (\c -> if c == child then insert key val child else c) children


lowerLeafs :: (Ord a) => a -> [Value a b] -> BTree a b
lowerLeafs key values = Leaf $ filterValues (<= key) values

higherLeafs :: (Ord a) => a -> [Value a b] -> BTree a b
higherLeafs key values = Leaf $ filterValues (> key) values

filterValues :: (a -> Bool) -> [Value a b] -> [Value a b]
filterValues func = filter (\(Value (a, _)) -> func a)

getMedianValue :: (Ord a) => [Value a b] -> Value a b
getMedianValue values = sort values !! getMedianValueIndex values

getMedianValueIndex :: [Value a b] -> Int
getMedianValueIndex = divideByTwo . length

getMedian :: (Ord a) => [a] -> a
getMedian keys = sort keys !! (length keys `div` 2)

divideByTwo :: Int -> Int
divideByTwo val = val `div` 2

getChildIndex ::(Ord k) => k -> [k] -> Int
getChildIndex key keys = case safeHead (getChildIndexes key keys) of
  Just a -> a
  Nothing -> length keys

getChildIndexes :: (Ord k) => k -> [k] -> [Int]
getChildIndexes key = findIndices (key <=)

refactor :: (Ord a) => [a] -> [BTree a b] -> BTree a b
refactor keys children
  | allSameType children = Node keys children
  | otherwise = refactorUpwords $ refactorInner keys children


refactorUpwords :: (Ord a) => BTree a b -> BTree a b
refactorUpwords leaf@(Leaf _) = leaf
refactorUpwords node@(Node keys values)
  | length keys <= treeSize = node
  | otherwise =
    let median = getMedian keys
    in Node [median] [
    Node (getLowerKeys median keys) (filter filterEmpty $ map (getLower median) values),
    Node (getHigherKeys median keys) (filter filterEmpty $ map (getHigher median) values)
    ]


getLowerKeys :: (Ord a) => a -> [a] -> [a]
getLowerKeys median = filter (< median)

getHigherKeys :: (Ord a) => a -> [a] -> [a]
getHigherKeys median = filter (> median)

getLower :: (Ord a) => a -> BTree a b -> BTree a b
getLower key tree = case tree of
  Node a b -> Node (getLowerKeys key a) (map (getLower key) b)
  Leaf a -> Leaf (filterValues (<= key) a)

getHigher :: (Ord a) => a -> BTree a b -> BTree a b
getHigher key tree = case tree of
  Node a b -> Node (getHigherKeys key a) (map (getHigher key) b)
  Leaf a -> Leaf (filterValues (> key) a)

filterEmpty :: (Ord a) => BTree a b -> Bool
filterEmpty tree = case tree of
  Leaf a -> not (null a)
  _ -> True

refactorInner ::(Ord a) => [a] -> [BTree a b] -> BTree a b
refactorInner keys children =
  let  (Node innK innC) = getInnerNode children
       leafs = getInnerLeafs children
  in Node (sort $ keys ++ innK) (sort $ innC ++ leafs)

getInnerNode :: [BTree a b] -> BTree a b
getInnerNode children = head $ getInnerNodes children

getInnerNodes :: [BTree a b] -> [BTree a b]
getInnerNodes = filter isNode

getInnerLeafs :: [BTree a b] -> [BTree a b]
getInnerLeafs = filter isLeaf

isNode :: BTree a b -> Bool
isNode tree = case tree of
  Node _ _ -> True
  _ -> False

isLeaf :: BTree a b -> Bool
isLeaf tree = case tree of
  Leaf _ -> True
  _ -> False

allSameType :: [BTree a b] -> Bool
allSameType children = all (sameType (head children)) children

sameType :: BTree a b -> BTree a b -> Bool
sameType (Leaf _) (Leaf _) = True
sameType (Node _ _) (Node _ _) = True
sameType _ _ = False