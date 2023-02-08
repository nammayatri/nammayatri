module Beckn.Types.Core.Taxi.API.OnInit where

import Beckn.Types.Core.Taxi.OnInit
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnInitReq = BecknCallbackReq OnInitMessage

type OnInitRes = AckResponse

type OnInitAPI =
  "on_init"
    :> ReqBody '[JSON] OnInitReq
    :> Post '[JSON] OnInitRes

onInitAPI :: Proxy OnInitAPI
onInitAPI = Proxy
