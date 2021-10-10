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
         in Node [medianValue] [lowerLeafs medianValue values, higherLeafs medianValue values]
insert key val (Node keys children) = 
  let index = getChildIndex key keys
      child = children !! index
  in Node keys $ map (\c -> if c == child then insert key val child else c) children


lowerLeafs :: (Ord a) => a -> [Value a b] -> BTree a b 
lowerLeafs key values = Leaf $ filterValues (<= key) values

higherLeafs :: (Ord a) => a -> [Value a b] -> BTree a b 
higherLeafs key values = Leaf $ filterValues (> key) values

filterValues :: (a -> Bool) -> [Value a b] -> [Value a b]
filterValues func = filter (\(Value (a, _)) -> func a)

getMedianValue :: (Ord a) => [Value a b] -> Value a b
getMedianValue values = sort values !! getMedianIndex values

getMedianIndex :: [Value a b] -> Int
getMedianIndex = divideByTwo . length

divideByTwo :: Int -> Int
divideByTwo val = val `div` 2

getChildIndex ::(Ord k) => k -> [k] -> Int
getChildIndex key keys = case safeHead (getChildIndexes key keys) of
  Just a -> a
  Nothing -> length keys

getChildIndexes :: (Ord k) => k -> [k] -> [Int]
getChildIndexes key = findIndices (key <=)