module ParserToken where


import Text.Parsec
import Data.Functor.Identity
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellDef, emptyDef)

import BdbValues


parseBool :: ParsecT String u Identity BValue
parseBool = do
  reserved "BBool"
  i <- identifier
  return $ BBool . makeBBool $ i

makeBBool :: String -> Bool
makeBBool a = case a of
  "true" -> True
  "false" -> False
  _ -> False


parseString :: ParsecT String u Identity BValue
parseString = do
  reserved "BString"
  i <- identifier
  return $ BString i


getValue :: ParsecT String u Identity String
getValue = (char ',' >> (many $ noneOf ")")) <|> return ""


lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser (emptyDef {T.reservedNames = T.reservedNames emptyDef ++ ["BNull", "BString", "BNumber", "BMap", "BBool", "BList"]})

identifier :: ParsecT String u Identity String
identifier = T.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved = T.reserved lexer

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = T.brackets lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = T.parens lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = T.commaSep lexer

testParse :: String -> Either ParseError BValue
testParse = parse parseBool "feil"