module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude
import Beckn.Types.Error

newtype AppEnv = AppEnv
  { commonEnv :: CommonEnv
  }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
