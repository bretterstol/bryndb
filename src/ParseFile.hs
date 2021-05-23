{-# LANGUAGE OverloadedStrings #-}

module ParseFile where

import Text.ParserCombinators.Parsec
import BdbValues

type ParseKeyValue = (String, String)
type ParseType = (String, ParseKeyValue)

convertTypes :: [[String]] -> [Maybe BValue]
convertTypes = map (convertStringToBValue . convertListToTuple)

convertListToTuple :: [String] -> (String, String)
convertListToTuple (x:y:_) = (x,y)

convertStringToBValue :: (String, String) -> Maybe BValue
convertStringToBValue (b, v) = case b of
  "BBool" -> convertToBool v
  "BNumber" -> convertToNumber v
  "BString" -> Just (BString v)
  _ ->  Nothing

parseBDB :: String -> Either ParseError BValue
parseBDB = parse parseFile "Feil"

parseFile :: GenParser Char st BValue
parseFile = do
  elements <- many parseLine
  eof
  return $ BMap elements

parseLine :: GenParser Char st (String, BValue)
parseLine = do
  key <- getKey
  values <- getType
  eol
  return (key, values)

getKey :: GenParser Char st String
getKey = many (noneOf " ")

getType :: GenParser Char st BValue
getType = (char ' ' >> (getKey >>= handleTypes)) <|> return BNull

handleTypes :: String -> GenParser Char st BValue
handleTypes t = case t of
  "BMap" -> (takeSpace >> handleMap) <|> return BNull
  "BString" -> ( takeSpace >> handleString) <|> return BNull
  _ -> return BNull

handleMap :: GenParser Char st BValue
handleMap = do
  char '['
  t <- many getValue
  eol
  return $ BMap t

takeSpace :: GenParser Char st Char
takeSpace = char ' '

handleString :: GenParser Char st BValue
handleString = do
  str <- many $ noneOf " )]\n"
  return $ BString str


eol :: GenParser Char st String
eol = string "\n"
--eol = try (string ")]") <|> try (string ")]") <|> try (string ")") <|> string "\n"

getValue :: GenParser Char st (String, BValue)
getValue = (char '(' >> parseLine) <|> return ("null", BNull)




getKeyValue :: GenParser Char st (String, BValue)
getKeyValue = do
  key <- readKey
  value <- handleTypes key
  return (key,value)

readKey :: GenParser Char st String
readKey = many $ noneOf " ,\n "

readValue :: GenParser Char st String
readValue = (char ',' >> many alphaNum) <|> return ""