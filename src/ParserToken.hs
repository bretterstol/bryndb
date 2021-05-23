module ParserToken where
import Text.Parsec
import Data.Functor.Identity
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (haskellDef)

import BdbValues

parseBool :: ParsecT String u Identity BValue
parseBool = do
  reserved "BBool"
  i <- identifier
  return $ BBool . makeBBool $ i

makeBBool :: [Char] -> Bool
makeBBool a = case a of 
  "true" -> True
  "false" -> False
  _ -> False

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser (haskellDef {T.reservedNames = T.reservedNames haskellDef ++ ["BNull", "BString", "BNumber", "BMap", "BBool", "BList"]})

identifier :: ParsecT String u Identity String
identifier = T.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved = T.reserved lexer

testParse :: String -> Either ParseError BValue
testParse = parse parseBool "feil" 