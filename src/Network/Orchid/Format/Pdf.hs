module Network.Orchid.Format.Pdf (fPdf) where

import Control.Monad (liftM)
import Data.ByteString.Lazy hiding (writeFile)
import Data.FileStore (FileStore)
import Network.Orchid.Core.Format
import Network.Protocol.Uri
import Prelude hiding (readFile)
import System.Process (waitForProcess, runProcess)
import Text.Document.Document

fPdf :: WikiFormat
fPdf = WikiFormat "pdf" "application/pdf" pdf

-- TODO: write to _cache dir, not /tmp
pdf :: FileStore -> FilePath -> FilePath -> String -> IO Output
pdf _ _ _ src = do
  writeFile "/tmp/tex-to-pdf.tex"
    $ either show toLaTeX
    $ fromWiki src
  runProcess "pdflatex" ["-interaction=batchmode", "/tmp/tex-to-pdf.tex"]
    (Just "/tmp") Nothing Nothing Nothing Nothing
    >>= waitForProcess
  liftM BinaryOutput $ readFile "/tmp/tex-to-pdf.pdf"

{-  runProcess "latexmk" [
      "-pdf", "-silent", "-f", "-g"
    ] (Just "/tmp") Nothing Nothing Nothing Nothing
    >>= waitForProcess-}
