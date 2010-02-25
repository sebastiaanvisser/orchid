module Network.Orchid.Format.Haskell (fHaskell) where

import Data.FileStore (FileStore)
import Network.Orchid.Core.Format
import Text.Document.Document

fHaskell :: WikiFormat
fHaskell = WikiFormat "hs" "text/plain" haskell

haskell :: FileStore -> FilePath -> FilePath -> String -> IO Output
haskell _ _ _ src = return 
  $ TextOutput
  $ either show show
  $ fromWiki src


