module AWS.S3.Types where

import Beckn.Prelude hiding (show)
import Beckn.Utils.Dhall (FromDhall)
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
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

data S3Config = S3AwsConf S3AwsConfig | S3MockConf S3MockConfig
  deriving (Generic, FromDhall)

data S3AwsConfig = S3AwsConfig
  { accessKeyId :: Text,
    secretAccessKey :: Text,
    bucketName :: Text,
    region :: Text,
    pathPrefix :: Text
  }
  deriving (Generic, FromDhall)

data S3MockConfig = S3MockConfig
  { baseLocalDirectory :: String,
    pathPrefix :: Text,
    bucketName :: Text
  }
  deriving (Generic, FromDhall)

data S3AuthParams = S3AuthParams
  { headers :: HttpTypes.RequestHeaders,
    queryString :: BS.ByteString,
    body :: BS.ByteString,
    path :: BS.ByteString,
    method :: HttpTypes.Method,
    date :: BS.ByteString
  }

data S3Env m = S3Env
  { pathPrefix :: Text,
    getH :: String -> m Text,
    putH :: String -> Text -> m ()
  }

get :: (MonadReader r m, HasField "s3Env" r (S3Env m)) => String -> m Text
get path = do
  s3env <- asks (.s3Env)
  getH s3env path

put :: (MonadReader r m, HasField "s3Env" r (S3Env m)) => String -> Text -> m ()
put path file_ = do
  s3env <- asks (.s3Env)
  putH s3env path file_
