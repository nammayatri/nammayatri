module Storage.Types
  ( StorageServiceConfig (..),
    GCSConfig (..),
    GCSGcpConfig (..),
    StorageEnv,
    module AWS.S3.Types,
  )
where

import AWS.S3.Types
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data StorageServiceConfig = StorageServiceConfig
  { isForcedAWS :: Bool,
    awsConfig :: Maybe S3Config,
    gcsConfig :: Maybe GCSConfig
  }
  deriving (Generic, FromDhall)

newtype GCSConfig = GCSGcpConf GCSGcpConfig
  deriving (Generic, FromDhall)

data GCSGcpConfig = GCSGcpConfig
  { gcpProjectId :: Text,
    bucketName :: Text,
    pathPrefix :: Text
  }
  deriving (Generic, FromDhall)

type StorageEnv m = S3Env m
