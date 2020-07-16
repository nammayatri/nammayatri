{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Status where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import EulerHS.Prelude

data StatusReq = StatusReq
  { context :: Context,
    message :: IdObject -- RIDEBOOK case.id
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse

data OnStatusReq = OnStatusReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnStatusRes = AckResponse
