module App.Types where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Data.Cache as C
import EulerHS.Prelude

data AppEnv = AppEnv
  { common :: CommonEnv,
    cache :: C.Cache Text Text
  }

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

instance HasCommonEnv Flow where
  getCommonEnv = asks common
