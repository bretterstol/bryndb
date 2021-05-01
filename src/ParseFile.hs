{-# LANGUAGE OverloadedStrings #-}

module ParseFile where

import Text.ParserCombinators.Parsec
import BdbValues

getFile :: String -> IO String
getFile = readFile

readObject :: GenParser Char st [[String]]
readObject = do
  types <- many readTypes
  eof
  return types

readTypes :: GenParser Char st [String]
readTypes = do
  types <- readType
  eol
  return types

readType :: GenParser Char st [String]
readType = do
  bType <- content
  bValue <- next
  return $ bType : bValue

content :: GenParser Char st String
content = many (noneOf " \n") 

next :: GenParser Char st [String]
next = (char ' ' >> readType) <|> return []

eol :: GenParser Char st Char
eol = char '\n'
  
parseB :: String -> Either ParseError [[String]]
parseB = parse readObject "Feil"

getResult :: Either ParseError [[String]] -> IO ()
getResult parsed = case parsed of
  Right list -> print $ convertTypes list
  Left b -> print b

testParse :: IO ()
testParse = do
  file <- getFile "test.bdb"
  getResult $ parseB file

convertTypes :: [[String]] -> [Maybe BValue]
convertTypes = map (convertStringToBValue . convertListToTup)

convertListToTup :: [String] -> (String, String)
convertListToTup (x:y:_) = (x,y)


