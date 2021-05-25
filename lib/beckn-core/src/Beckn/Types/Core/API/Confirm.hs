{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Confirm
  ( module Beckn.Types.Core.API.Confirm,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] AckResponse

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
