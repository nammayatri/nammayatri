{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Types.Config where

import EulerHS.Extra.Aeson
import EulerHS.Prelude

data RedisConfig = RedisConfig
  { _host :: Text,
    _port :: Word16,
    _db :: Integer,
    _password :: Maybe Text,
    _socket_keep_alive :: Bool
  }
  deriving (Generic, Eq, Show)

instance FromJSON RedisConfig where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON RedisConfig where
  toJSON = genericToJSON stripAllLensPrefixOptions

data DatabaseConfig = DatabaseConfig
  { _host :: String,
    _port :: Word16,
    _db :: String,
    _user :: String,
    _password :: String
  }
  deriving (Generic, Eq, Show)

instance FromJSON DatabaseConfig where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON DatabaseConfig where
  toJSON = genericToJSON stripAllLensPrefixOptions

data DBPoolDetails = DBPoolDetails
  { _pool :: Int,
    _maxIdleTime :: Int,
    _maxConnections :: Int
  }
  deriving (Generic, Eq, Show)

instance FromJSON DBPoolDetails where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON DBPoolDetails where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Config = Config
  { _database :: DatabaseConfig,
    _dbPoolDetails :: DBPoolDetails,
    _redis :: RedisConfig,
    _isMaskingEnabled :: Bool
  }

data DBSyncConfig = DBSyncConfig
  { _emptyRetry :: Int,
    _rateLimitN :: Int,
    _rateLimitWindow :: Int,
    _streamReadCount :: Integer
  }
