module Network.Orchid.Format.Html (fHtml) where

import Control.Monad (liftM)

import Data.FileStore (FileStore)
import Network.Orchid.Core.Format
import Network.Protocol.Uri
import Text.Document.Document

fHtml :: WikiFormat
fHtml = WikiFormat "html" "text/html" html

html :: FileStore -> FilePath -> FilePath -> String -> IO Output
html _ work _ src = do
  case fromWiki src of
    Left err  -> return $ TextOutput (show err)
    Right doc -> liftM (TextOutput . show) (processToXHTML work doc)

