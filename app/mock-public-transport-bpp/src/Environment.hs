module Environment where

import Beckn.Types.Logging
import Beckn.Utils.CacheHedis
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Relude
import Servant.Client

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    redisPrefix :: Text,
    statusWaitTimeSec :: Int,
    callbackWaitTimeMilliSec :: Int,
    loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildAppEnv :: HedisEnv -> LoggerEnv -> AppCfg -> AppEnv
buildAppEnv hedisEnv loggerEnv config = AppEnv {..}
