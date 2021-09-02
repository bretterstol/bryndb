module FileSearch where

import BdbValues
import ParserFull
import Search
import System.Directory
import Control.Concurrent.Async
import Data.Either

search :: (String, BValue) -> IO [Either String BValue]
search searchKeyValue = do
  filePaths <- listDirectory "./data"
  files <- mapM readBDFile filePaths
  return $ searchFiles searchKeyValue files

searchFiles :: (String,BValue) -> [String] -> [Either String BValue]
searchFiles key = map (searchFile key)

searchFile :: (String,BValue) -> String -> Either String BValue
searchFile key file = mapLeft (const "Parse failed") (parseFile file) >>= find key

searchFileIO :: (String,BValue) -> IO String -> IO (Either String BValue)
searchFileIO key ioFile = searchFile key <$> ioFile


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft func val = case val of
  Left a -> Left $ func a
  Right b -> Right b

searchAsync :: (String, BValue) -> IO [BValue]
searchAsync keyVal = do
  filePaths <- listDirectory "./data"
  result <- readAllFiles keyVal filePaths
  return $ rights result
  
readAllFiles :: (String, BValue) -> [FilePath] -> IO [Either String BValue]
readAllFiles keyVal = runConcurrently . traverse (Concurrently . searchFileIO keyVal . readBDFile) 

readBDFile :: FilePath -> IO String
readBDFile = readFile . ("./data/" ++)

