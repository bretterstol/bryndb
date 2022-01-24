module File.FileInsert where

import BdbValues
import Utils
import qualified BValueSearch as S
import Data.UUID (UUID)

insert :: BValue -> IO String
insert values = do
  (bString@(BString index), withIndex) <- insertIndex values
  _ <- writeToFile bString withIndex
  return index

writeToFile :: BValue -> BValue -> IO ()
writeToFile index vals = writeFile (getFilePath index) (show vals)

getFilePath :: BValue -> String
getFilePath index = case index of
  BString i -> "./data/" ++ i ++ ".bdb"
  _ -> "./data/undefined"

insertIndex :: BValue -> IO (BValue, BValue)
insertIndex vals = case checkIfHasId vals of
  Nothing -> addId vals
  Just a -> return (a, vals)

checkIfHasId :: BValue -> Maybe BValue
checkIfHasId = S.getKeyValue "_id"

addId :: BValue -> IO (BValue, BValue)
addId vals = do
  uuid <- getUUid
  return (uuidToBValue uuid, concatId ("_id", BString $ show uuid) vals)

uuidToBValue :: UUID -> BValue
uuidToBValue uuid = BString $ show uuid

concatId :: (String, BValue) -> BValue -> BValue
concatId id vals = case vals of
  BMap val -> BMap $ id:val
  _ -> vals