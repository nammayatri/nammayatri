{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Status where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.Order
-- import Beckn.Types.Mobility.Service
import EulerHS.Prelude

data StatusReq = StatusReq
  { context :: Context,
    message :: StatusReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse

data OnStatusReq = OnStatusReq
  { context :: Context,
    message :: OnStatusReqMessage,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnStatusRes = AckResponse

data StatusReqMessage = StatusReqMessage
  { service :: IdObject,
    order :: IdObject
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype OnStatusReqMessage = OnStatusReqMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)
