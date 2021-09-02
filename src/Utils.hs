module Utils where

import Data.UUID (UUID)
import Data.UUID.V4

getUUid :: IO UUID
getUUid = nextRandom

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x