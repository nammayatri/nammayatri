{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module AWS.S3.Types where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text as T
import qualified Data.Text.Encoding as DTE
import Data.Time.Format.ISO8601 (iso8601Show)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude hiding (show)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types as HttpTypes
import Servant

data FileType = Audio | Video | Image | AudioLink | VideoLink | ImageLink | PortraitVideoLink
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''FileType)

data S3OctetStream = S3OctetStream deriving (Typeable)

instance Accept S3OctetStream where
  contentType _ = "application" M.// "octet-stream"

instance MimeRender S3OctetStream Text where
  mimeRender _ = BSL.fromStrict . DTE.encodeUtf8

instance MimeUnrender S3OctetStream Text where
  mimeUnrender _ = pure . DTE.decodeUtf8 . BSL.toStrict

data S3ImageData = S3ImageData deriving (Typeable)

instance Accept S3ImageData where
  contentType _ = "image" M.// "png"

instance MimeRender S3ImageData Text where
  mimeRender _ = BSL.fromStrict . DTE.encodeUtf8

instance MimeUnrender S3ImageData Text where
  mimeUnrender _ = pure . DTE.decodeUtf8 . BSL.toStrict

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

createFilePath ::
  ( MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3Env m)
  ) =>
  Text ->
  Text ->
  FileType ->
  Text ->
  m Text
createFilePath domain identifier fileType validatedFileExtention = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> domain <> identifier <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> validatedFileExtention
    )

get :: (MonadReader r m, HasField "s3Env" r (S3Env m)) => String -> m Text
get path = do
  s3env <- asks (.s3Env)
  getH s3env path

put :: (MonadReader r m, HasField "s3Env" r (S3Env m)) => String -> Text -> m ()
put path file_ = do
  s3env <- asks (.s3Env)
  putH s3env path file_

getPublic :: (MonadReader r m, HasField "s3EnvPublic" r (S3Env m)) => String -> m Text
getPublic path = do
  s3EnvPublic <- asks (.s3EnvPublic)
  getH s3EnvPublic path

putPublic :: (MonadReader r m, HasField "s3EnvPublic" r (S3Env m)) => String -> Text -> m ()
putPublic path file_ = do
  s3EnvPublic <- asks (.s3EnvPublic)
  putH s3EnvPublic path file_
