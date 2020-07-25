{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Update where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Mobility.Order
import EulerHS.Prelude

data UpdateReq = UpdateReq
  { context :: Context,
    message :: UpdateReq
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data UpdateOrder = UpdateOrder
  { update_action :: Maybe String, -- UPDATE-PICKUP | UPDATE-DROP | ADD-STOP
    service :: Maybe UpdateOrderService,
    order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype UpdateOrderService = UpdateOrderService
  { id :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type UpdateRes = AckResponse

data OnUpdateReq = OnUpdateReq
  { context :: Context,
    message :: OnUpdateOrder,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype OnUpdateOrder = OnUpdateOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnUpdateRes = AckResponse
