{-# LANGUAGE FlexibleContexts #-}
module Network.Orchid.Core.Handler
( FileStoreType (..)
, hRepository
, hViewer
, hWiki
, hWikiCustomViewer
)
where

import Control.Applicative
import Control.Monad.Trans
import Data.FileStore hiding (NotFound)
import Network.Orchid.Core.Format
import Network.Orchid.Core.Liaison
import Network.Orchid.FormatRegister
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Paths_orchid

data FileStoreType = Git | Darcs

mkFileStore :: FileStoreType -> FilePath -> FileStore
mkFileStore Git   = gitFileStore
mkFileStore Darcs = darcsFileStore

-- todo: clean up this mess:

hRepository
  :: (BodyM Request m, HttpM' m, MonadIO m, SendM m, LoginM m p)
  => FileStoreType -> FilePath -> FilePath -> m ()
hRepository kind repo dir =
  let fs = mkFileStore kind repo in
    hPath "/search" (post (hWikiSearch fs))
  $ hPrefix "/_images" (hFileSystem (repo /+ "_images"))
  $ hPrefix "/_cache"  (hFileSystem (repo /+ "_cache"))
  $ hFileTypeDispatcher
      hDirectoryResource
      (\_ -> hWithoutDir repo (hWikiREST dir fs))
      repo 

hViewer
  :: (MonadIO m, HttpM' m, SendM m, QueueM m, BodyM Request m, Alternative m)
  => FilePath -> m ()
hViewer dir =
  hPath "/"
   (hFileResource (dir /+ "show.html"))
   (hExtendedFileSystem dir)

hWiki
  :: (MonadIO m, BodyM Request m, LoginM m p, Alternative m, QueueM m, SendM m, HttpM' m)
  => FileStoreType -> FilePath -> FilePath -> m ()
hWiki kind repo dir = do
  viewerDir <- liftIO (getDataFileName "viewer")
  hWikiCustomViewer viewerDir kind repo dir

hWikiCustomViewer
  :: (LoginM m p, Alternative m, QueueM m, SendM m, MonadIO m, HttpM' m, BodyM Request m)
  => FilePath -> FileStoreType -> FilePath -> FilePath -> m ()
hWikiCustomViewer viewerDir kind repo dir = 
  hPrefix "/data"
    (hRepository kind repo dir)
    (authHandlers (hViewer viewerDir))

authHandlers :: (LoginM m p, HttpM' m, SendM m) => m () -> m ()
authHandlers =
  hPathRouter
    [ ("/loginfo", authorized (Just "loginfo") forbidden (const loginfo))
    , ("/login",   post (login forbidden (const ok)))
    , ("/logout",  post logout)
    , ("/signup",  post (signup ["loginfo", "show", "edit", "create"] forbidden (const ok)))
    ]

ok :: (HttpM Response m, SendM m) => m ()
ok = hCustomError OK "ok"

post :: (HttpM Response m, SendM m, HttpM Request m) => m () -> m ()
post h = hMethod POST h (hError NotFound)

forbidden :: (HttpM Response m, SendM m) => m ()
forbidden = hCustomError Forbidden "No authorized to perform this action"

hWikiREST
  :: (HttpM' m, BodyM Request m, SendM m, MonadIO m, LoginM m p)
  => FilePath -> FileStore -> m ()
hWikiREST dir fs =
  hUri $ \u ->
      previewHandlers u
    . actionHandlers  u
    $ hError BadRequest
  where

  previewHandlers u = hPathRouter
    $ map (\ext -> ("/preview." ++ ext, hWikiRetrieve fs dir True u))
    (map postfix wikiFormats) 

  actionHandlers u =
    hMethodRouter [
      (GET,                                                   hWikiRetrieve       fs dir False u )
    , (PUT,    authorized (Just "edit")   forbidden (\user -> hWikiStore          fs user      u))
    , (DELETE, authorized (Just "delete") forbidden (\user -> hWikiDeleteOrRename fs user      u))
    ]

