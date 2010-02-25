module Network.Orchid.Format.History (fHistory) where

import Data.FileStore (FileStore (history), Author (..), TimeRange (..), Revision (..))
import Data.List (intercalate)
import Network.Orchid.Core.Format (Output (..), WikiFormat (..))

fHistory :: WikiFormat
fHistory = WikiFormat "his" "text/plain" his

his :: FileStore -> FilePath -> FilePath -> String -> IO Output
his filestore _ p _ = do

  -- Get all the history for one single document.
  revs <-  history filestore [dropWhile (=='/') p] noTimeRange
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

