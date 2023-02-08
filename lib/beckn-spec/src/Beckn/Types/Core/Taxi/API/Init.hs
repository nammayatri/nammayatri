module Beckn.Types.Core.Taxi.API.Init where

import Beckn.Types.Core.Taxi.Init
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type InitReq = BecknReq InitMessage

type InitRes = AckResponse

type InitAPI =
  "init"
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] InitRes

initAPI :: Proxy InitAPI
initAPI = Proxy
