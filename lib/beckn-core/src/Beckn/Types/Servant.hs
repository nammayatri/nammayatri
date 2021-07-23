module Beckn.Types.Servant where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text
import Data.Text.Encoding
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import qualified Network.HTTP.Media as M
import Servant

data PlainText_ISO_8859_1 deriving (Typeable)

instance Accept PlainText_ISO_8859_1 where
  contentType _ = "text" M.// "plain" M./: ("charset", "ISO-8859-1")

instance MimeUnrender PlainText_ISO_8859_1 Text where
  mimeUnrender _ = Right . decodeLatin1 . toStrict

instance MimeRender PlainText_ISO_8859_1 Text where
  mimeRender _ = fromStrict . encodeUtf8
