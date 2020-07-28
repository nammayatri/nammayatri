module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import Control.Monad.Reader

newtype AppEnv = AppEnv
  { db :: DbEnv
  }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance HasDbEnv Flow where
  getDbEnv = asks db
