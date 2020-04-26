module Beckn.Types.App where

import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

-- App Types
data Env =
  Env
    { runTime :: R.FlowRuntime
    }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)
