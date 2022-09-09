module BdbValues where

import Text.Read (readMaybe)


data BValue = 
  BNull |
  BNumber Float |
	BString String |
	BBool Bool |
	BList [BValue] |
	BMap [(String, BValue)]
	deriving (Eq, Show, Ord)



convertToBool :: String -> Maybe BValue
convertToBool b = case b of
  "true" -> Just (BBool True)
  "false" -> Just (BBool False)
  _ -> Nothing
  
convertToNumber :: String -> Maybe BValue
convertToNumber n = do
   let num = readMaybe n :: Maybe Float
   case num of 
     Just a -> Just (BNumber a)
     Nothing -> Nothing
