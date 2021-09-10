module Beckn.Types.Servant where

import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import qualified Network.HTTP.Media as M
import Servant

data PlainText_ISO_8859_1 deriving (Typeable)

instance Accept PlainText_ISO_8859_1 where
  contentType _ = "text" M.// "plain" M./: ("charset", "ISO-8859-1")
