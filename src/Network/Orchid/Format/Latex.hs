module Network.Orchid.Format.Latex (fLatex) where

import Data.FileStore (FileStore)
import Network.Orchid.Core.Format
import Network.Protocol.Uri
import Text.Document.Document

fLatex :: WikiFormat
fLatex = WikiFormat "tex" "text/plain" latex

latex :: FileStore -> FilePath -> FilePath -> String -> IO Output
latex _ _ _ src = return 
  $ TextOutput
  $ either show toLaTeX
  $ fromWiki src

