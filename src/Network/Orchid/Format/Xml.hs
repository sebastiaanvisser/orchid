module Network.Orchid.Format.Xml (fXml) where

import Text.XML.Light.Output

import Data.FileStore (FileStore)
import Network.Orchid.Core.Format
import Text.Document.Document

fXml :: WikiFormat
fXml = WikiFormat "xml" "text/xml" xml

xml :: FileStore -> FilePath -> FilePath -> String -> IO Output
xml _ _ _ src = return
  $ TextOutput
  $ either show (ppContent . toXML)
  $ fromWiki src

