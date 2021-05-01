module Main where

import ParseFile
import BdbValues

main :: IO ()
main = testParse

getTest :: BValue
getTest = BObject [("Test", BObject [
	("TestTall", BNumber 1),
	("TestString", BString "test"),
	("InnerObj", BObject [("InnerList", BList [
		(BBool True)])])])]

printTest :: IO()
printTest = print getTest
