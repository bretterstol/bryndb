module Search where
import BdbValues


find :: String -> BValue -> Either String BValue
find searchKey testVal = if keyIsInBValue searchKey testVal then Right testVal else Left "Not found"

keyIsInBValue :: String -> BValue -> Bool
keyIsInBValue searchKey testVal = Just True == fmap (searchList searchKey) (getList testVal)

searchList :: String -> [(String, BValue)] -> Bool
searchList searchKey val = hasElem $ map (findKey searchKey) val

hasElem :: [Bool] -> Bool
hasElem a = isNotEmpty $ filter id a

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty _ = True

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