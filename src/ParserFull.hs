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
parseMap = between leftBracket rightBracket $ BMap <$> getTupeList

getTupeList :: GenParser Char st [(String, BValue)]
getTupeList = do
  first <- parseTup
  rest <- nextTup
  return $ first : rest


nextTup :: GenParser Char st [(String, BValue)]
nextTup = (comma >> getTupeList) <|> return [] <?> "Nextup failed"

endOfMap :: GenParser Char st Char
endOfMap = try comma <|> rightBracket <?> "End failed parseMap"

parseTup :: GenParser Char st (String, BValue)
parseTup = between leftParens rightParens getTupes

getTupes :: GenParser Char st (String, BValue)
getTupes = do 
  key <- many $ noneOf ","
  comma
  t <- many $ noneOf " "
  whiteSpace
  value <- handleTypes t
  return (key, value)

handleTypes :: String -> GenParser Char st BValue
handleTypes t = case t of
  "BString" -> parseString
  "BNumber" -> parseNumber
  "BBool" -> parseBool
  "BMap" -> parseMap
  _ -> return BNull

parseString :: GenParser Char st BValue
parseString = between quote quote $ BString <$> many (noneOf "\"")

parseNumber :: GenParser Char st BValue
parseNumber = getNum <$> many alphaNum

parseBool :: GenParser Char st BValue
parseBool = getBool <$> many letter

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