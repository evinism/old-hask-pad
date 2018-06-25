{-# LANGUAGE OverloadedStrings #-}

module NoteHandlers where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Map.Syntax ((##))
import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import qualified Heist.Interpreted as I
---- local imports ----
import           Application
import           Note

{-
  TODO: find a better place for this (copied from )
  pg table init command (this is totally the right way to do things like this):
  // ---
  CREATE TABLE notes (
    id varchar(8) PRIMARY KEY,
    lastuse timestamp NOT NULL,
    content text NOT NULL
  );
-}

bsToText :: BS.ByteString -> T.Text
bsToText bs = T.pack $ BS.unpack bs

renderNote :: Note -> Handler App App ()
renderNote note = heistLocal (I.bindSplice "noteContent" spl) $ render "note_page"
  where spl = I.textSplice $ noteContent note

-- Todo: put these into their own files
getNote :: NoteId -> Handler App App ()
getNote noteId = do
  results <- query "select id, content from notes WHERE id=?" $ Only noteId
  case results of
    [] -> writeText "404 - no note of that ID"
    (note:_) -> renderNote note

handleGetNote :: Handler App App ()
handleGetNote = do
  potNoteId <- getParam "noteId"
  case potNoteId of
    Nothing -> writeText "404 - no note of that ID"
    Just noteId -> getNote $ bsToText noteId


handleNewNote :: Handler App App ()
handleNewNote = do
  noteId <- liftIO randomNoteId
  execute "INSERT INTO notes VALUES (?, now(), '');" $ Only noteId
  redirect $ BS.pack ("/note/" ++ (T.unpack noteId))

-- put these into notes, but for now it ain't necessary
updateNote :: NoteId -> T.Text -> Handler App App()
updateNote noteId content = do
  results <- query "UPDATE notes SET content=?, lastuse=now() WHERE id=?;" $ (content, noteId)
  case results of
    [] -> writeText "404 - no note of that ID"
    (note:_) -> writeText $ noteContent note

handleUpdateNote :: Handler App App ()
handleUpdateNote = do 
  potNoteId <- getParam  "noteId"
  potContent <- getParam "content"
  params <- return $ liftM2 (,) potNoteId potContent
  case params of
    Nothing -> writeText "404 - no note of that ID"
    Just (noteId, content) -> updateNote (bsToText noteId) (bsToText content)