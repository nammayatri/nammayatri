{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Call where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import EulerHS.Prelude

data CallReq = CallReq
  { context :: Context,
    message :: IdObject -- ProductInstance id
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CallRes = AckResponse
