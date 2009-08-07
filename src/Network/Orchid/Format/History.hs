module Network.Orchid.Format.History (fHistory) where

import Control.Monad (liftM)
import Data.FileStore (FileStore (history), Author (..), TimeRange (..), Revision (..))
import Data.List (intercalate)
import Data.Record.Label (lget)
import Network.Orchid.Core.Format (Output (..), WikiFormat (..))
import Network.Protocol.Uri (URI, path, mkPathRelative)
import Text.Document.Document ()

fHistory :: WikiFormat
fHistory = WikiFormat "his" "text/plain" his

his :: FileStore -> FilePath -> FilePath -> String -> IO Output
his filestore _ p _ = do

  -- Get all the history for one single document.
  revs <-  history filestore [mkPathRelative p] noTimeRange
  return
    $ TextOutput
    $ intercalate "\n\n"
    $ map showRevision
      revs

noTimeRange :: TimeRange
noTimeRange = TimeRange Nothing Nothing

showRevision :: Revision -> String
showRevision rev =
    intercalate "\n"
  $ map (\(a, b) -> a ++ "=" ++ b rev) [
      ("date",        show . revDateTime)
    , ("author",      authorName  . revAuthor)
    , ("email",       authorEmail . revAuthor)
    , ("description", revDescription)
    , ("id",          revId)
    ]

