module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude

newtype AppEnv = AppEnv
  { common :: CommonEnv
  }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance HasCommonEnv Flow where
  getCommonEnv = asks common
