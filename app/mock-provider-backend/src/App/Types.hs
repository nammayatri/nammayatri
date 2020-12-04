module App.Types where

import Beckn.Storage.DB.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Logging
import EulerHS.Prelude

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    selfId :: Text,
    nwAddress :: BaseUrl,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
