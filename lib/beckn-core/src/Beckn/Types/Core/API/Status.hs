{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Status
  ( module Beckn.Types.Core.API.Status,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Common (IdObject (..))
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Order
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] StatusReq
    :> Post '[JSON] StatusRes

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
