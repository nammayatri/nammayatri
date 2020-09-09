module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Logging
import qualified Data.Cache as C
import EulerHS.Prelude
import qualified EulerHS.Types as T

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Maybe Text,
    nwAddress :: Maybe BaseUrl,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: Maybe LoggerConfig,
    searchTimeout :: Maybe Int,
    traceFlag :: TraceFlag
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    gwId :: Maybe Text,
    gwNwAddress :: Maybe BaseUrl,
    cache :: C.Cache Text Text,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: Maybe LoggerConfig,
    searchTimeout :: Maybe Int, -- In seconds
    traceFlag :: TraceFlag
  }
  deriving (Generic)

mkAppEnv :: AppCfg -> C.Cache Text Text -> AppEnv
mkAppEnv AppCfg {..} c =
  AppEnv
    { gwId = selfId,
      gwNwAddress = nwAddress,
      cache = c,
      ..
    }

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api
