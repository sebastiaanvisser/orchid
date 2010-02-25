module Network.Orchid.Format.Plain (fPlain) where

import Data.FileStore (FileStore)
import Network.Orchid.Core.Format

fPlain :: WikiFormat
fPlain = WikiFormat "txt" "text/plain" plain

plain :: FileStore -> FilePath -> FilePath -> String -> IO Output
plain _ _ _ = return . TextOutput

