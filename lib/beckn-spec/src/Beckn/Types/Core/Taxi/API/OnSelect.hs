module Beckn.Types.Core.Taxi.API.OnSelect where

import Beckn.Types.Core.Taxi.OnSelect (OnSelectMessage)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnSelectReq = BecknCallbackReq OnSelectMessage

type OnSelectRes = AckResponse

type OnSelectAPI =
  "on_select"
    :> ReqBody '[JSON] OnSelectReq
    :> Post '[JSON] OnSelectRes

onSelectAPI :: Proxy OnSelectAPI
onSelectAPI = Proxy
