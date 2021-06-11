module FileSearch where

import BdbValues
import ParserFull
import Search
import System.Directory

search :: (String, BValue) -> IO [Either String BValue]
search searchKeyValue = do
  filePaths <- listDirectory "./data"
  files <- mapM (readFile . ("./data/" ++)) filePaths
  return $ searchFiles searchKeyValue files

searchFiles :: (String,BValue) -> [String] -> [Either String BValue]
searchFiles key = map (searchFile key)

searchFile :: (String,BValue) -> String -> Either String BValue
searchFile key file = mapLeft (const "Teit ass") (parseFile file) >>= find key

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft func val = case val of
  Left a -> Left $ func a
  Right b -> Right b