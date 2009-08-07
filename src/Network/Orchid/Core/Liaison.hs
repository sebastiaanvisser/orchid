module Network.Orchid.Core.Liaison (
    hWikiRetrieve
  , hWikiDeleteOrRename
  , hWikiStore
  , hWikiSearch
  ) where

import Control.Applicative
import Control.Exception.Extensible
import Control.Monad.State
import Data.Encoding
import Data.Encoding.UTF8
import Data.FileStore hiding (NotFound)
import Data.List hiding (delete)
import Data.Record.Label
import Misc.Commons
import Network.Orchid.Core.Format (WikiFormat (..), Output (..))
import Network.Orchid.FormatRegister
import Network.Protocol.Http hiding (body)
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Httpd hiding (email)
import qualified Data.ByteString.Lazy as B

-------- showing wiki documents -----------------------------------------------

{- |
Dependent on the `b' flag we either bounce the PUTted contents back in the
requested format, or we open the source version of the requested resource and
print this back in the requested format.
-}

hWikiRetrieve
  :: (Receive m, MonadIO m, Send m, Response m)
  => FileStore -> FilePath -> Bool -> URI -> m ()
hWikiRetrieve filestore workDir b u = do

  -- Fetch revision identifier from URI query string and convert this to a
  -- Maybe based on string-emptyness.
  let rev = lget query u
      revid = if null rev then Nothing else Just rev

  -- Compute the source file and format handler.
  let src = mkPathRelative $ lset extension Nothing $ lget path u
      ext = maybe "txt" id $ lget (extension % path) u
      fmt = maybe defaultFormat id $ find ((ext==) . postfix) wikiFormats

  -- The body might be retrieved from our filestore or from the request itself.
  body <- if b
    then contentsUtf8
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
        TextOutput   s' -> (encodeLazyByteString UTF8 s', Just utf8)
        BinaryOutput bs -> (bs, Nothing)

      response $
        do setM status OK
           setM contentType   (mime fmt, enc)
           setM contentLength (Just $ fromIntegral $ B.length body')
      sendBs body'

-------- deleting wiki documents ----------------------------------------------

-- TODO:generalize deletion/storage

hWikiDeleteOrRename
  :: (MonadIO m, Response m, Send m, Receive m)
  => FileStore -> User -> URI -> m ()
hWikiDeleteOrRename filestore user u = do

  let rev = lget query u

  if null rev
    then hCustomError BadRequest errEmptyRev
    else do
      mdoc <- contentsUtf8
      let aut = Author (username user) (email user)
          src = mkPathRelative $ lset extension Nothing $ lget path u
      liftIO $ case mdoc of
        Nothing -> delete filestore src    aut rev
        Just mv -> rename filestore src mv aut rev

errEmptyRev, errEmptyRes :: String
errEmptyRev = "empty revision name not allowed"
errEmptyRes = "empty resource name not allowed"

-------- storing wiki documents -----------------------------------------------

hWikiStore
  :: (Receive m, MonadIO m, Response m, Send m)
  => FileStore -> User -> URI -> m ()
hWikiStore filestore user u = do

  let rev = lget query u
  mdoc <- contentsUtf8
  case (null rev, mdoc) of

    -- Error cases.
    (True,  Nothing) -> hCustomError BadRequest (errEmptyRev ++ "\n" ++ errEmptyRes)
    (True,  Just _)  -> hCustomError BadRequest errEmptyRev
    (False, Nothing) -> hCustomError BadRequest errEmptyRes

    (False, Just doc) -> liftIO $ do
      let aut = Author (username user) (email user)
          src = mkPathRelative $ lset extension Nothing $ lget path u
      save filestore src aut rev doc

-------- searching wiki documents ---------------------------------------------

hWikiSearch :: (Receive m, MonadIO m, Response m, Send m) => FileStore -> m ()
hWikiSearch filestore = do

  params <- uriEncodedPostParamsUTF8

  case getSearchInfo params of
    Nothing -> hCustomError BadRequest "no search query specified"
    Just (a, b, c, d) -> do
      res <- liftIO $ search filestore (SearchQuery [a] b c d)
      response $
        do setM status OK
           setM contentType ("text/plain", Just utf8)
      sendStr (intercalate "\n\n" $ map showMatch res)

getSearchInfo :: Maybe Parameters -> Maybe (String, Bool, Bool, Bool)
getSearchInfo params = do
  p <- params
  patterns   <- "patterns"   `lookup` p >>= id
  wholewords <- "wholewords" `lookup` p >>= id >>= safeRead
  matchall   <- "matchall"   `lookup` p >>= id >>= safeRead
  ignorecase <- "ignorecase" `lookup` p >>= id >>= safeRead
  return (patterns, wholewords, matchall, ignorecase)

showMatch :: SearchMatch -> String
showMatch match =
    intercalate "\n"
  $ map (\(a, b) -> a ++ "=" ++ b match) [
      ("resource",    matchResourceName)
    , ("linenumber",  show . matchLineNumber)
    , ("line",        matchLine)
    ]

