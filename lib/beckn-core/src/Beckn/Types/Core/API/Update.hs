{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Update
  ( module Beckn.Types.Core.API.Update,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type UpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

onUpdate :: Proxy UpdateAPI
onUpdate = Proxy

data UpdateReq = UpdateReq
  { context :: Context,
    message :: UpdateReq
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateOrder = UpdateOrder
  { update_action :: Maybe String, -- UPDATE-PICKUP | UPDATE-DROP | ADD-STOP
    service :: Maybe UpdateOrderService,
    order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype UpdateOrderService = UpdateOrderService
  { id :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type UpdateRes = AckResponse

type OnUpdateReq = CallbackReq OnUpdateOrder

newtype OnUpdateOrder = OnUpdateOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type OnUpdateRes = AckResponse
