module Search where
import BdbValues
import Data.Maybe (isJust)


find :: String -> BValue -> Maybe BValue
find searchKey testVal = if keyIsInBValue searchKey testVal then Just testVal else Nothing

keyIsInBValue :: String -> BValue -> Bool
keyIsInBValue searchKey testVal = isJust $ fmap (searchList searchKey) (getList testVal)

searchList :: String -> [(String, BValue)] -> Bool
searchList searchKey val = hasElem $ map (findKey searchKey) val

hasElem :: [a] -> Bool
hasElem [] = False
hasElem _ = True

findKey :: String -> (String, BValue) -> Bool
findKey searchKey (key, val)
  | key == searchKey = True
  | isMap val = keyIsInBValue searchKey val
  | otherwise = False

isMap :: BValue -> Bool
isMap a = case a of
  BMap _ -> True
  _ -> False

getList :: BValue -> Maybe [(String, BValue)]
getList b = case b of
  BMap a -> Just a
  _ -> Nothing