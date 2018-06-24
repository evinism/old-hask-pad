{-# LANGUAGE OverloadedStrings #-}

module NoteHandlers where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Map.Syntax ((##))
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


handleNote :: Handler App App ()
handleNote = do
  results <- query_ "select * from snap_auth_user"
  liftIO $ print (results :: [AuthUser])
  render "note_page"

handleNewNote :: Handler App App ()
handleNewNote = do
  noteId <- liftIO randomNoteId
  redirect $ BS.pack ("/note/" ++ (T.unpack noteId))
