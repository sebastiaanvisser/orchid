module Network.Orchid.FormatRegister (
    wikiFormats
  , defaultFormat
  ) where

import Network.Orchid.Core.Format (WikiFormat)
import Network.Orchid.Format.Html (fHtml)
import Network.Orchid.Format.Xml (fXml)
import Network.Orchid.Format.Haskell (fHaskell)
import Network.Orchid.Format.History (fHistory)
import Network.Orchid.Format.Latex (fLatex)
import Network.Orchid.Format.Pdf (fPdf)
import Network.Orchid.Format.Plain (fPlain)

wikiFormats :: [WikiFormat]
wikiFormats = [
    fPdf
  , fXml
  , fHtml
  , fPlain
  , fLatex
  , fHaskell
  , fHistory
  ] 

defaultFormat :: WikiFormat
defaultFormat = fPlain

