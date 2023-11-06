{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.DBSync
  ( Env (..),
    Types.DBSync.Flow,
    History,
    StateRef (..),
    DBSyncException (..),
    DBCommand (..),
    CreateDBCommand (..),
    UpdateDBCommand (..),
    DeleteDBCommand (..),
  )
where

import Data.Aeson (Object)
import Database.Beam.Postgres (Connection)
import EulerHS.KVConnector.DBSync
import EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types as ET hiding (Tag)
import Kafka.Producer as Producer
import Types.Config
import Types.Event as Event

data Env = Env
  { _streamRedisInfo :: Text,
    _counterHandles :: Event.DBSyncCounterHandler,
    _kafkaConnection :: Producer.KafkaProducer,
    _pgConnection :: Connection,
    _dontEnableDbTables :: [Text],
    _dontEnableForKafka :: [Text]
  }

type Flow = EL.ReaderFlow Env

type History = [Int]

data StateRef = StateRef
  { _config :: DBSyncConfig,
    _history :: History
  }

data DBSyncException
  = DECODE_ERROR Text
  | CONFIG_NOT_FOUND Text
  | REDIS_STREAM_ERROR ET.KVDBReply
  deriving (Show)

instance Exception DBSyncException

data DBCommand
  = Create DBCommandVersion Tag Double DBName Object
  | Update DBCommandVersion Tag Double DBName Object
  | Delete DBCommandVersion Tag Double DBName Object
  deriving (Generic, ToJSON, FromJSON)

data CreateDBCommand = CreateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

data UpdateDBCommand = UpdateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

data DeleteDBCommand = DeleteDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName Object

deriving stock instance Show EL.KVDBStreamEntryID

deriving stock instance Eq EL.KVDBStreamEntryID
