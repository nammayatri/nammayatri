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
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] AckResponse

confirm :: Proxy ConfirmAPI
confirm = Proxy

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirm :: Proxy OnConfirmAPI
onConfirm = Proxy

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: ConfirmOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type ConfirmRes = AckResponse

type OnConfirmReq = CallbackReq ConfirmOrder

newtype ConfirmOrder = ConfirmOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type OnConfirmRes = AckResponse
