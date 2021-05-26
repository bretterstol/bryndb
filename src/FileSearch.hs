module FileSearch where

import BdbValues
import ParserFull
import Search
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Data.Either
import GHC.Base (IO)

search :: String -> IO [Either String BValue]
search key = do
  filePaths <- listDirectory "./data"
  files <- mapM (readFile . ("./data/" ++)) filePaths
  return $ searchFiles key files

searchFiles :: String -> [String] -> [Either String BValue]
searchFiles key = map (searchFile key)

searchFile :: String -> String -> Either String BValue
searchFile key file = mapLeft (const "ParseError") (parseFile file) >>= find key

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft func val = case val of
  Left a -> Left $ func a
  Right b -> Right b