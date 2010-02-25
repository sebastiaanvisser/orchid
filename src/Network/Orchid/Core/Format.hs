module Network.Orchid.Core.Format where

import Data.ByteString.Lazy (ByteString)
import Data.FileStore (FileStore)

-- Formats produce proper UTF-8 text or binary docs. (TODO: ascii for tex?)
data Output =
    TextOutput   String
  | BinaryOutput ByteString

-- Wiki format description data type.
data WikiFormat =
  WikiFormat {
    postfix :: String
  , mime    :: String
  , handler :: FileStore
            -> FilePath   -- Working dir.
            -> FilePath   -- Document name.
            -> String     -- Contents.
            -> IO Output
  }

