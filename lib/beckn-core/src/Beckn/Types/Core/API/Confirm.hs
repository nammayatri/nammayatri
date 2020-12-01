{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Confirm where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import Beckn.Utils.Servant.HeaderAuth
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmAPI v =
  "confirm"
    :> APIKeyAuth v
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
