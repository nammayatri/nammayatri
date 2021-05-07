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
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] AckResponse

cancel :: Proxy CancelAPI
cancel = Proxy

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

onCancel :: Proxy OnCancelAPI
onCancel = Proxy

data CancelReq = CancelReq
  { context :: Context,
    message :: CancelReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

type OnCancelReq = CallbackReq OnCancelReqMessage

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

newtype OnCancelReqMessage = OnCancelReqMessage
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)
