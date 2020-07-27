{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Confirm where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Mobility.Order
import EulerHS.Prelude

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: ConfirmOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type ConfirmRes = AckResponse

data OnConfirmReq = OnConfirmReq
  { context :: Context,
    message :: ConfirmOrder,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype ConfirmOrder = ConfirmOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnConfirmRes = AckResponse
