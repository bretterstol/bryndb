module Search where
import BdbValues
import Data.Maybe (mapMaybe)



find :: String -> BValue -> Maybe BValue
find searchKey testVal = getList testVal >>= searchList searchKey


searchList :: String -> [(String, BValue)] -> Maybe BValue
searchList searchKey val = getFirst $ mapMaybe (findKey searchKey) val

getFirst :: [a] -> Maybe a
getFirst [] = Nothing
getFirst (a:_) = Just a

findKey :: String -> (String, BValue) -> Maybe BValue
findKey searchKey (key, val)
  | key == searchKey = Just val
  | isMap val = find searchKey val
  | otherwise = Nothing

isMap :: BValue -> Bool
isMap a = case a of
  BMap _ -> True
  _ -> False

getList :: BValue -> Maybe [(String, BValue)]
getList b = case b of
  BMap a -> Just a
  _ -> Nothing