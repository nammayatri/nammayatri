module Beckn.Utils.Servant.JSONBS where

import qualified Data.ByteString.Lazy as BS
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import qualified Network.HTTP.Media as M
import Servant

data JSONBS deriving (Typeable)

instance Accept JSONBS where
  contentTypes _ =
    "application" M.// "json" M./: ("charset", "utf-8")
      NE.:| ["application" M.// "json"]

instance MimeRender JSONBS ByteString where
  mimeRender _ = BS.fromStrict

instance MimeUnrender JSONBS ByteString where
  mimeUnrender _ = pure . BS.toStrict
