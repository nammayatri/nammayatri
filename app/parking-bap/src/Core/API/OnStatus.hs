module Core.API.OnStatus where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.API.Types (BecknCallbackReq)
import Core.OnStatus (OnStatusMessage)
import Servant (JSON, Post, ReqBody, (:>))

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OnStatusMessage)
    :> Post '[JSON] AckResponse

onStatusAPI :: Proxy OnStatusAPI
onStatusAPI = Proxy
