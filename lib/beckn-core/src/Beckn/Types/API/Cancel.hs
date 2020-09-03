{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Cancel where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Descriptor
import Beckn.Types.Mobility.Order
import Beckn.Types.Mobility.Trip
import EulerHS.Prelude

data CancelReq = CancelReq
  { context :: Context,
    message :: CancelReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

type OnCancelReq = CallbackReq Trip

type OnCancelRes = AckResponse

data CancelReqMessage = CancelReqMessage
  { cancellation :: Cancellation,
    order :: CancellationOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Cancellation = Cancellation
  { id :: Text,
    reason :: Maybe Descriptor
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancellationOrder = CancellationOrder
  { id :: Text,
    cancellation_reason_id :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnCancelReqMessage = OnCancelReqMessage
  { cancellation_id :: Text,
    order :: Order
  }
