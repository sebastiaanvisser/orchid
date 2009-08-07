module Network.Orchid.Core.Handler (
    FileStoreType (..)
  , hRepository
  , hViewer
  , hWiki
  , hWikiCustomViewer
  ) where

import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Monad.State
import Data.Record.Label
import Misc.Commons ((.$))
import Data.FileStore (FileStore (..), darcsFileStore, gitFileStore)
import Network.Orchid.Core.Format (postfix)
import Network.Orchid.Core.Liaison (hWikiStore, hWikiDeleteOrRename, hWikiRetrieve, hWikiSearch)
import Network.Orchid.FormatRegister (wikiFormats)
import Network.Protocol.Http (Method (..), Status (..), uri)
import Network.Protocol.Uri ((/+), URI(..), path)
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Paths_orchid

-------- main entry point -----------------------------------------------------

data FileStoreType = Git | Darcs

mkFileStore :: FileStoreType -> FilePath -> FileStore
mkFileStore Git   = gitFileStore
mkFileStore Darcs = darcsFileStore

hPOST h = hMethodRouter [(POST, h)] (hError NotFound)

-- todo: clean up this mess:

hRepository
  :: (Response m, Send m, Request m, Receive m, MonadIO m)
  => FileStoreType -> FilePath -> FilePath -> UserDatabase b -> TUserSession a -> m ()
hRepository kind repo workDir userdb session =
  let fs = mkFileStore kind repo in
    hPath "/search" (hAuthorized userdb "search" (\_ -> const () `fmap` (hPOST $ hWikiSearch fs)) session)
  $ hPrefix "/_" (hFileSystem (repo /+ "_"))
  $ hFileTypeDispatcher hDirectoryResource
  ( const
  $ hWithoutDir repo
  $ hWikiREST workDir userdb session fs)
  repo 

hViewer
  :: (Request m, MonadIO m, Response m, Send m, Socket m)
  => FilePath -> m ()
hViewer dir = do
  hPath "/"
   .$ hFileResource (dir /+ "show.html")
    $ hExtendedFileSystem dir

hWiki
  :: (MonadIO m, Receive m, Socket m, Response m, Send m, Request m)
  => FileStoreType -> FilePath -> FilePath -> TVar (UserDatabase FilePath) -> TUserSession a -> m ()
hWiki kind repo workDir userdb session = do
  viewerDir <- liftIO $ getDataFileName "viewer"
  hWikiCustomViewer viewerDir kind repo workDir userdb session

hWikiCustomViewer
  :: (MonadIO m, Socket m, Response m, Send m, Request m, Receive m)
  => FilePath -> FileStoreType -> FilePath -> FilePath -> TVar (UserDatabase FilePath) -> TUserSession a -> m ()
hWikiCustomViewer viewerDir kind repo workDir tuserdb session = do
  userdb <- liftIO . atomically $ readTVar tuserdb
  hPrefix "/data"
    (hRepository kind repo workDir userdb session)
    (authHandlers tuserdb session $ hViewer viewerDir)

authHandlers
  :: (MonadIO m, Receive m, Send m, Response m, Request m)
  => TVar (UserDatabase FilePath) -> TUserSession a -> m () -> m ()
authHandlers tuserdb session handler = do
  userdb <- liftIO . atomically $ readTVar tuserdb
  hPathRouter [
      ("/loginfo", (hAuthorized userdb "loginfo" (const $ hLoginfo session) session) >> return ())
    , ("/login",   (hPOST $ hLogin userdb session) >> return ())
    , ("/logout",  (hPOST $ hLogout session) >> return ())
    , ("/signup",   hAuthorized userdb "signup" (const (hPOST (hSignup tuserdb ["loginfo", "show", "edit", "create"]) >> return ())) session)
    ] handler

-------- REST interface -------------------------------------------------------

-- The wiki module will act as a REST interface by using the MethodRouter
-- handler to dispatch on the HTTP request method.

hWikiREST
  :: (Request m, Receive m, MonadIO m, Send m, Response m)
  => FilePath -> UserDatabase b -> TUserSession a -> FileStore -> m ()
hWikiREST workDir userdb session filestore =
  hUri $ \uri ->
  previewHandlers filestore workDir uri
    $ actionHandlers  filestore workDir uri userdb session
    $ hError BadRequest

actionHandlers
  :: (Receive m, MonadIO m, Send m, Response m, Request m)
  => FileStore -> FilePath -> URI -> UserDatabase b -> TUserSession a -> m () -> m ()
actionHandlers filestore workDir uri userdb session =
  hMethodRouter [
    (GET,    hAuthorized userdb "show"   (const $ hWikiRetrieve filestore workDir False uri) session)
  , (PUT,    hAuthorizedUser    "edit"   (flip (hWikiStore          filestore) uri) session)
  , (DELETE, hAuthorizedUser    "delete" (flip (hWikiDeleteOrRename filestore) uri) session)
  ]

previewHandlers
  :: (Receive m, MonadIO m, Send m, Response m, Request m)
  => FileStore -> FilePath -> URI -> m () -> m ()
previewHandlers filestore workDir uri = hPathRouter (
    map (\ext -> ("/preview." ++ ext, hWikiRetrieve filestore workDir True uri))
  $ map postfix wikiFormats) 

