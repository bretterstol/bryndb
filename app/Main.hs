module Main where

import Search
import BdbValues

testVal :: BValue
testVal = BMap [("Test",BString "test"),("Ny test",BNumber 3),("nested",BMap [("Inni",BString "HEI PÅ DEG")]),("nummer", BNumber 3)]

main :: IO ()
main = do print $ find "" testVal

