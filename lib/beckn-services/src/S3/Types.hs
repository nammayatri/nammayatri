module S3.Types where

import Beckn.Prelude hiding (show)
import Beckn.Utils.Dhall (FromDhall)
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import EulerHS.Prelude hiding (decodeUtf8, show, traceShowId)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types as HttpTypes
import Servant

data S3OctetStream = S3OctetStream deriving (Typeable)

instance Accept S3OctetStream where
  contentType _ = "application" M.// "octet-stream"

instance MimeRender S3OctetStream Text where
  mimeRender _ = BSL.fromStrict . T.encodeUtf8

instance MimeUnrender S3OctetStream Text where
  mimeUnrender _ = pure . T.decodeUtf8 . BSL.toStrict

data S3ImageData = S3ImageData deriving (Typeable)

instance Accept S3ImageData where
  contentType _ = "image" M.// "png"

instance MimeRender S3ImageData Text where
  mimeRender _ = BSL.fromStrict . T.encodeUtf8

instance MimeUnrender S3ImageData Text where
  mimeUnrender _ = pure . T.decodeUtf8 . BSL.toStrict

data S3Config = S3Config
  { accessKeyId :: Text,
    secretAccessKey :: Text,
    bucketName :: Text,
    region :: Text
  }
  deriving (Generic, FromDhall)

class S3AuthenticatingEntity r where
  getSecretAccessKey :: r -> Text
  getAccessKeyId :: r -> Text
  getBucketName :: r -> Text
  getRegion :: r -> Text

data S3AuthParams = S3AuthParams
  { headers :: HttpTypes.RequestHeaders,
    queryString :: BS.ByteString,
    body :: BS.ByteString,
    path :: BS.ByteString,
    method :: HttpTypes.Method,
    date :: BS.ByteString
  }
