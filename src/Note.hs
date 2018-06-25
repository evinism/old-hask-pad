{-# LANGUAGE OverloadedStrings #-}

module Note where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

type NoteError = T.Text
type NoteResult = Either NoteError Note
type NoteId = T.Text

data Note = Note
  { id :: NoteId
  , noteContent :: T.Text
  } deriving (Show)

instance FromRow Note where
  fromRow = Note <$> field <*> field

noteExists :: Connection -> NoteId -> IO Bool
noteExists = error "todo: noteExists"

writeNote :: Connection -> Note -> IO (Maybe NoteError)
writeNote = error "todo: writeNote"

readNote :: Connection -> NoteId -> IO NoteResult
readNote = error "todo: readNote"

randomNoteId :: IO NoteId
randomNoteId = do
    uuid <- nextRandom
    let tUuid = T.pack $ toString uuid in
      return $ T.take 8 tUuid