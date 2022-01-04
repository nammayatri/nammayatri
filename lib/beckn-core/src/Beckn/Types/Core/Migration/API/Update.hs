module Beckn.Types.Core.Migration.API.Update where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Order
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type UpdateAPI =
  "update"
    :> ReqBody '[JSON] (BecknReq UpdateInfo)
    :> Post '[JSON] AckResponse

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

data UpdateInfo = UpdateInfo
  { update_target :: Text,
    order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnUpdateAPI =
  "on_update"
    :> ReqBody '[JSON] (BecknCallbackReq OrderObject)
    :> Post '[JSON] AckResponse

onUpdateAPI :: Proxy OnUpdateAPI
onUpdateAPI = Proxy
