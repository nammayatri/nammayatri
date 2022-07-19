module Beckn.Types.Core.Taxi.API.Init where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Init
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type InitReq = BecknReq InitMessage

type InitRes = AckResponse

type InitAPI =
  "init"
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] InitRes

initAPI :: Proxy InitAPI
initAPI = Proxy
