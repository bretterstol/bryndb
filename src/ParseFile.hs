{-# LANGUAGE OverloadedStrings #-}

module ParseFile where

import Text.Parsec
import Data.Text

getFile :: String -> IO String
getFile = readFile



