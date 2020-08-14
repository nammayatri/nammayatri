{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Status where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Order
import EulerHS.Prelude

data StatusReq = StatusReq
  { context :: Context,
    message :: StatusReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse

type OnStatusReq = CallbackReq OnStatusReqMessage

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
