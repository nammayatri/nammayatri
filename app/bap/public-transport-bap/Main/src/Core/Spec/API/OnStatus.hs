module Core.Spec.API.OnStatus where

import Core.Spec.OnStatus (OnStatusMessage)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes
import Servant (JSON, Post, ReqBody, (:>))

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OnStatusMessage)
    :> Post '[JSON] AckResponse

onStatusAPI :: Proxy OnStatusAPI
onStatusAPI = Proxy
