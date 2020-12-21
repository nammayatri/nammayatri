{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Update where

import Beckn.Types.Core.API.Auth
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import qualified Beckn.Types.Core.Order as Core
import Beckn.Types.FMD.Order
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type UpdateAPI v =
  BecknAuth
    v
    ( "update"
        :> ReqBody '[JSON] UpdateReq
        :> Post '[JSON] UpdateRes
    )

updateAPI :: Proxy (UpdateAPI v)
updateAPI = Proxy

type OnUpdateAPI v =
  BecknAuth
    v
    ( "on_update"
        :> ReqBody '[JSON] OnUpdateReq
        :> Post '[JSON] OnUpdateRes
    )

onUpdateAPI :: Proxy (OnUpdateAPI v)
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
