{-# LANGUAGE FlexibleContexts #-}
module Network.Orchid.Core.Liaison (
    hWikiRetrieve
  , hWikiDeleteOrRename
  , hWikiStore
  , hWikiSearch
  ) where

import Control.Applicative
import Control.Category
import Control.Exception.Extensible
import Control.Monad.State hiding (get)
import Data.FileStore hiding (NotFound)
import Data.List hiding (delete)
import Data.Record.Label
import Network.Orchid.Core.Format (WikiFormat (..), Output (..))
import Network.Orchid.FormatRegister
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Protocol.Uri.Parser ()
import Network.Salvia.Handlers
import Network.Salvia.Httpd hiding (body)
import Prelude hiding ((.), id)
import Safe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U

-------- showing wiki documents -----------------------------------------------

{- |
Dependent on the `b' flag we either bounce the PUTted contents back in the
requested format, or we open the source version of the requested resource and
print this back in the requested format.
-}

hWikiRetrieve :: (BodyM Request m, SendM m, MonadIO m, HttpM Response m) => FileStore -> FilePath -> Bool -> Uri -> m ()
hWikiRetrieve filestore workDir b u = do

  -- Fetch revision identifier from URI query string and convert this to a
  -- Maybe based on string-emptyness.
  let rev = get query u
      revid = if null rev then Nothing else Just rev

  -- Compute the source file and format handler.
  let src = dropWhile (=='/') $ set extension Nothing $ get path u
      ext = maybe "txt" id $ get (extension . path) u
      fmt = maybe defaultFormat id $ find ((ext==) . postfix) wikiFormats

  -- The body might be retrieved from our filestore or from the request itself.
  body <- if b
    then Just <$> hRequestBodyStringUTF8
    else liftIO (
          either (\e -> const Nothing (e::FileStoreError)) Just 
      <$> try (smartRetrieve filestore False src revid))

  -- Format the body using the selected wiki handler or return an error when
  -- the body could not be retrieved.
  case body of
    Nothing -> hError NotFound
    Just s -> do
      b' <- liftIO $ (handler fmt) filestore workDir src s
      (body', enc) <- return $ case b' of
        TextOutput   s' -> (U.fromString s', Just "utf-8")
        BinaryOutput bs -> (bs, Nothing)

      response $
        do setM status OK
           setM contentType   (Just (mime fmt, enc))
           setM contentLength (Just $ B.length body')
      sendBs body'

-------- deleting wiki documents ----------------------------------------------

-- TODO:generalize deletion/storage

hWikiDeleteOrRename :: (MonadIO m, HttpM Response m, SendM m, BodyM Request m) => FileStore -> User -> Uri -> m ()
hWikiDeleteOrRename filestore user u = do

  let rev = get query u

  if null rev
    then hCustomError BadRequest errEmptyRev
    else do
      doc <- hRequestBodyStringUTF8
      let aut = Author (get username user) (get email user)
          src = dropWhile (=='/') $ set extension Nothing $ get path u
      liftIO $ rename filestore src doc aut rev

errEmptyRev :: String
errEmptyRev = "empty revision name not allowed"

-------- storing wiki documents -----------------------------------------------

hWikiStore :: (BodyM Request m, MonadIO m, HttpM Response m, SendM m) => FileStore -> User -> Uri -> m ()
hWikiStore filestore user u = do

  let rev = get query u
  doc <- hRequestBodyStringUTF8
  case null rev of

    True  -> hCustomError BadRequest errEmptyRev
    False -> liftIO $ do
      let aut = Author (get username user) (get email user)
          src = dropWhile (=='/') $ set extension Nothing $ get path u
      save filestore src aut rev doc

-------- searching wiki documents ---------------------------------------------

hWikiSearch :: (BodyM Request m, HttpM' m, MonadIO m, SendM m) => FileStore -> m ()
hWikiSearch filestore = do

  ps <- hRequestParameters "utf-8"
  case getSearchInfo ps of
    Nothing -> hCustomError BadRequest "no search query specified"
    Just (a, b, c, d) -> do
      res <- liftIO $ search filestore (SearchQuery [a] b c d)
      response $
        do setM status OK
           setM contentType (Just ("text/plain", Just "utf-8"))
      send (intercalate "\n\n" $ map showMatch res)

getSearchInfo :: Parameters -> Maybe (String, Bool, Bool, Bool)
getSearchInfo p = do
  patterns   <- "patterns"   `lookup` p >>= id
  wholewords <- "wholewords" `lookup` p >>= id >>= readMay
  matchall   <- "matchall"   `lookup` p >>= id >>= readMay
  ignorecase <- "ignorecase" `lookup` p >>= id >>= readMay
  return (patterns, wholewords, matchall, ignorecase)

showMatch :: SearchMatch -> String
showMatch match =
    intercalate "\n"
  $ map (\(a, b) -> a ++ "=" ++ b match) [
      ("resource",    matchResourceName)
    , ("linenumber",  show . matchLineNumber)
    , ("line",        matchLine)
    ]

