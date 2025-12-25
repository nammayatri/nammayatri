{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.DBSync
  ( module X,
    Env (..),
    Types.DBSync.Flow,
    History,
    StateRef (..),
    DBSyncConfig (..),
    DBCommand (..),
    CreateDBCommand (..),
    UpdateDBCommand (..),
    DeleteDBCommand (..),
    AppCfg (..),
  )
where

import Data.Aeson (Object)
import qualified Data.Aeson as A
import Data.Pool (Pool)
import Database.Beam.Postgres (Connection)
import EulerHS.KVConnector.DBSync
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import Kafka.Producer as Producer
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis.Config
import Kernel.Streaming.Kafka.Producer.Types
import qualified Kernel.Types.Common as KTC
import Kernel.Types.Logging
import Kernel.Utils.Dhall (FromDhall)
import Types.DBSync.Create as X
import Types.DBSync.Delete as X
import Types.DBSync.Update as X
import Types.Event as Event

data DBSyncConfig = DBSyncConfig
  { _emptyRetry :: Int,
    _rateLimitN :: Int,
    _rateLimitWindow :: Int,
    _streamReadCount :: Integer
  }

data Env = Env
  { _streamRedisInfo :: Text,
    _counterHandles :: Event.DBSyncCounterHandler,
    _kafkaConnection :: Producer.KafkaProducer,
    -- _pgConnection :: Connection,
    _dontEnableDbTables :: [Text],
    _dontEnableForKafka :: [Text],
    _connectionPool :: Pool Connection,
    _esqDBCfg :: EsqDBConfig
  }

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    kvConfigUpdateFrequency :: Int,
    kafkaProducerCfg :: KafkaProducerCfg,
    loggerConfig :: LoggerConfig,
    dontEnableForDb :: [Text],
    dontEnableForKafka :: [Text],
    kafkaProperties :: [KTC.KafkaProperties]
  }
  deriving (Generic, FromDhall)

type Flow = EL.ReaderFlow Env

type History = [Int]

data StateRef = StateRef
  { _config :: DBSyncConfig,
    _history :: History
  }

data DBCommand
  = Create DBCommandObject
  | Update DBCommandObject
  | Delete DBCommandObject
  deriving (Generic)

instance FromJSON DBCommand where
  parseJSON = genericParseJSON dbCommandOptions

dbCommandOptions :: A.Options
dbCommandOptions =
  A.defaultOptions
    { A.sumEncoding = dbCommandTaggedObject
    }

dbCommandTaggedObject :: A.SumEncoding
dbCommandTaggedObject =
  A.TaggedObject
    { tagFieldName = "tag",
      contentsFieldName = "contents_v2"
    }

data DBCommandObject = DBCommandObject
  { cmdVersion :: DBCommandVersion,
    tag :: Tag,
    timestamp :: Double,
    dbName :: DBName,
    command :: Object
  }
  deriving (Generic, FromJSON)

data CreateDBCommand = CreateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

data UpdateDBCommand = UpdateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

data DeleteDBCommand = DeleteDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

deriving stock instance Show EL.KVDBStreamEntryID

deriving stock instance Eq EL.KVDBStreamEntryID
