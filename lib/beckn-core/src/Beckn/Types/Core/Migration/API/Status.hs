module Beckn.Types.Core.Migration.API.Status where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.API.Types (BecknCallbackReq, BecknReq, OrderObject)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] (BecknReq OrderId)
    :> Post '[JSON] AckResponse

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

newtype OrderId = OrderId
  { order_id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OrderObject)
    :> Post '[JSON] AckResponse

onStatusAPI :: Proxy OnStatusAPI
onStatusAPI = Proxy
