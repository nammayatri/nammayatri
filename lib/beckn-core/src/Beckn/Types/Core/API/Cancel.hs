{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Cancel
  ( module Beckn.Types.Core.API.Cancel,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import Beckn.Types.Mobility.Trip
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] AckResponse

data CancelReq = CancelReq
  { context :: Context,
    message :: CancelReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

type OnCancelReq = CallbackReq Trip

type OnCancelRes = AckResponse

newtype CancelReqMessage = CancelReqMessage
  { order :: CancellationOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancellationOrder = CancellationOrder
  { id :: Text,
    cancellation_reason_id :: Maybe Text -- "CASE", "PRODUCT_INSTANCE"
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnCancelReqMessage = OnCancelReqMessage
  { cancellation_id :: Text,
    order :: Order
  }
