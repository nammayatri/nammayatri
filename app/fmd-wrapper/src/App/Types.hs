module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.FMD.Service
import Beckn.Utils.Common
import EulerHS.Prelude

data AppEnv = AppEnv
  { dbEnv :: DbEnv,
    redisEnv :: RedisEnv
  }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

newtype OnSearchServices = OnSearchServices
  { services :: [Service]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example OnSearchServices where
  example =
    OnSearchServices
      { services = example
      }

instance HasDbEnv Flow where
  getDbEnv = asks dbEnv

instance HasRedisEnv Flow where
  getRedisEnv = asks redisEnv
