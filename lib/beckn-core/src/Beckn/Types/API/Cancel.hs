{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Cancel where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Error
import Beckn.Types.Mobility.Order
import Beckn.Types.Mobility.Trip
import EulerHS.Prelude

data CancelReq = CancelReq
  { context :: Context,
    message :: CancelReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse

data OnCancelReq = OnCancelReq
  { context :: Context,
    message :: Trip,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnCancelRes = AckResponse

data CancelReqMessage = CancelReqMessage
  { cancellation :: Cancellation,
    order :: CancellationOrderId
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Cancellation = Cancellation
  { id :: Text,
    reason :: Maybe Descriptor
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype CancellationOrderId = CancellationOrderId
  { id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnCancelReqMessage = OnCancelReqMessage
  { cancellation_id :: Text,
    order :: Order
  }
