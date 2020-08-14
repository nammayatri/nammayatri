{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Confirm where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import EulerHS.Prelude

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: ConfirmOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type ConfirmRes = AckResponse

type OnConfirmReq = CallbackReq ConfirmOrder

newtype ConfirmOrder = ConfirmOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnConfirmRes = AckResponse
