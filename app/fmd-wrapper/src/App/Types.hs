module App.Types where

import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Logging
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Wrapper (DunzoConfig)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    port :: Int,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: Maybe LoggerConfig,
    coreVersion :: Text,
    domainVersion :: Text,
    dzConfig :: DunzoConfig
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
