module Beckn.Types.Core.Taxi.API.OnUpdate where

import Beckn.Types.Core.Taxi.OnUpdate (OnUpdateMessage)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnUpdateReq = BecknCallbackReq OnUpdateMessage

type OnUpdateRes = AckResponse

type OnUpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

onUpdateAPI :: Proxy OnUpdateAPI
onUpdateAPI = Proxy
