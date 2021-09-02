module BdbValues where

import Text.Read (readMaybe)


data BValue =
	BNull |
	BNumber Float |
	BString String |
	BBool Bool |
	BList [BValue] |
	BMap [(String, BValue)]
	deriving (Eq, Show)

--instance Show BValue where
--  show value = case value of
--    BNull -> "null"
--    BNumber a -> show a
--    BString a -> a
--    BBool True -> "true"
--    BBool False -> "false"
--    BList as -> intercalate ", " (map show as)
--    BObject as -> intercalate ", " (map showO as)
--	where showO (n, v) = "[" ++ n ++ " : " ++ show v ++ "]"



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
