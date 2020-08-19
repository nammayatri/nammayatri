module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude

newtype AppEnv = AppEnv
  { dbEnv :: DbEnv
  }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance HasDbEnv Flow where
  getDbEnv = asks dbEnv
