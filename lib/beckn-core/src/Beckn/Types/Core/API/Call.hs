{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Call where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import EulerHS.Prelude

data CallReq = CallReq
  { context :: Context,
    message :: IdObject -- ProductInstance id
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CallRes = AckResponse
