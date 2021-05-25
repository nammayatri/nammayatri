{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Update where

import Beckn.Types.Core.Ack
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Context
import Types.Beckn.FmdOrder
import qualified Types.Beckn.Order as Core

type UpdateAPI =
  "update"
    :> ReqBody '[JSON] UpdateReq
    :> Post '[JSON] UpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

type OnUpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

onUpdateAPI :: Proxy OnUpdateAPI
onUpdateAPI = Proxy

data UpdateReq = UpdateReq
  { context :: Context,
    message :: UpdateReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type UpdateRes = AckResponse

type OnUpdateReq = CallbackReq UpdateResMessage

data UpdateReqMessage = UpdateReqMessage
  { update_action :: Text, --"update_pickup_location", "update_pickup_poc", "update_pickup_instructions", "update_drop_location", "update_drop_poc", "update_drop_instructions", "update_package", "update_billing"
    order :: Core.Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnUpdateRes = AckResponse

newtype UpdateResMessage = UpdateResMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
