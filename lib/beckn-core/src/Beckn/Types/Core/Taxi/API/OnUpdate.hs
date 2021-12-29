module Beckn.Types.Core.Taxi.API.OnUpdate where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.OnUpdate (OnUpdateMessage)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnUpdateReq = BecknReq OnUpdateMessage

type OnUpdateRes = AckResponse

type OnUpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

onUpdateAPI :: Proxy OnUpdateAPI
onUpdateAPI = Proxy
