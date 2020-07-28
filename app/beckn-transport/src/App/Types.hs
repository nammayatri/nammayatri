module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude

data AppEnv = AppEnv
  { db :: DbEnv,
    redis :: RedisEnv
  }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance HasDbEnv Flow where
  getDbEnv = asks db

instance HasRedisEnv Flow where
  getRedisEnv = asks redis
