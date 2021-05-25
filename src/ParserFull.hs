module ParserFull where

import Text.ParserCombinators.Parsec
import BdbValues
import Control.Monad (liftM2)
import Text.Read (readMaybe)

testParse :: String -> Either ParseError BValue
testParse = parse startParse "feil"

startParse :: GenParser Char st BValue
startParse = do
  start <- many $ noneOf " "
  whiteSpace
  handleTypes start

parseMap :: GenParser Char st BValue
parseMap = do
  leftBracket
  res <- getTupes
  rightBracket
  return $ BMap $ res

getTupes :: GenParser Char st [(String, BValue)]
getTupes = do
  first <- parseTup
  rest <- nextTup
  return $ first : rest


nextTup :: GenParser Char st [(String, BValue)]
nextTup = (comma >> getTupes) <|> return [] <?> "Nextup failed"

endOfMap :: GenParser Char st Char
endOfMap = try comma <|> rightBracket <?> "End failed parseMap"

parseTup :: GenParser Char st (String, BValue)
parseTup = do
  leftParens
  key <- many $ noneOf ","
  comma
  t <- many $ noneOf " "
  whiteSpace
  value <- handleTypes t
  rightParens
  return (key, value)

handleTypes :: String -> GenParser Char st BValue
handleTypes t = case t of
  "BString" -> parseString
  "BNumber" -> parseNumber
  "BBool" -> parseBool
  "BMap" -> parseMap
  _ -> return BNull

parseString :: GenParser Char st BValue
parseString = do
  quote
  st <- many $ noneOf "\""
  quote
  return $ BString st

parseNumber :: GenParser Char st BValue
parseNumber = do
   anum <- many alphaNum
   return $ getNum anum

parseBool :: GenParser Char st BValue
parseBool = do
  b <- many letter
  return $ getBool b

getBool :: String -> BValue
getBool b = maybe BNull BBool (readBool b)

readBool :: String -> Maybe Bool
readBool b = case b of
  "True" -> Just True
  "False" -> Just False
  _ -> Nothing

getNum :: String -> BValue
getNum anum = maybe BNull BNumber (readNum anum)

readNum :: String -> Maybe Float
readNum anum = readMaybe anum :: Maybe Float

quote :: GenParser Char st Char
quote = char '"'

comma :: GenParser Char st Char
comma = char ','

leftParens :: GenParser Char st Char
leftParens = char '('

rightParens :: GenParser Char st Char
rightParens = char ')'

leftBracket :: GenParser Char st Char
leftBracket = char '['

rightBracket :: GenParser Char st Char
rightBracket = char ']'

whiteSpace :: GenParser Char st Char
whiteSpace = char ' '