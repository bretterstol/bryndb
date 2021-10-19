module BValueSearch where
import BdbValues
import Data.Maybe (catMaybes)
import Utils

find :: (String, BValue) -> BValue -> Either String BValue
find (searchKey,searchVal) doc = if keyValueFound then Right doc else Left "Not found"
  where keyValueFound = checkValue keyValue searchVal
        keyValue = getKeyValue searchKey doc

checkValue :: Maybe BValue -> BValue -> Bool
checkValue keyVal searchVal = case keyVal of
  Just kv -> kv == searchVal
  _ -> False

getKeyValue :: String -> BValue -> Maybe BValue
getKeyValue searchKey doc = takeKeyValue $ fmap (searchList searchKey) (getList doc)

takeKeyValue :: Maybe [Maybe BValue] -> Maybe BValue
takeKeyValue vals = vals >>= getResult . catMaybes

getResult :: [BValue] -> Maybe BValue
getResult [] = Nothing
getResult a = if length a > 1 then Nothing else safeHead a

searchList :: String -> [(String, BValue)] -> [Maybe BValue]
searchList searchKey = map (findKey searchKey)

findKey :: String -> (String, BValue) -> Maybe BValue
findKey searchKey (key, val)
  | key == searchKey = Just val
  | isMap val = getKeyValue searchKey val
  | otherwise = Nothing

isMap :: BValue -> Bool
isMap a = case a of
  BMap _ -> True
  _ -> False

getList :: BValue -> Maybe [(String, BValue)]
getList b = case b of
  BMap a -> Just a
  _ -> Nothing