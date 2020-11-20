{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Cancel where

import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Servant.HeaderAuth
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CancelAPI v =
  "cancel"
    :> APIKeyAuth v
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
