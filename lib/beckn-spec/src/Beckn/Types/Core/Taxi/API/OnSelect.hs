module Beckn.Types.Core.Taxi.API.OnSelect where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import Beckn.Types.Core.Taxi.OnSelect (OnSelectMessage)
import Servant (JSON, Post, ReqBody, (:>))

type OnSelectReq = BecknCallbackReq OnSelectMessage

type OnSelectRes = AckResponse

type OnSelectAPI =
  "on_select"
    :> ReqBody '[JSON] OnSelectReq
    :> Post '[JSON] OnSelectRes

onSelectAPI :: Proxy OnSelectAPI
onSelectAPI = Proxy
