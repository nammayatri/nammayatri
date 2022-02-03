module Environment where

import Beckn.Utils.CacheHedis
import Beckn.Utils.Dhall (FromDhall)
import Relude
import Servant.Client

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    redisPrefix :: Text,
    statusWaitTimeSec :: Int,
    callbackWaitTimeMilliSec :: Int
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    hedisEnv :: HedisEnv
  }
  deriving (Generic)

buildAppEnv :: HedisEnv -> AppCfg -> AppEnv
buildAppEnv hedisEnv config = AppEnv {..}
